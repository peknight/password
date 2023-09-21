package com.peknight.password.gen.charsets

import cats.data.{EitherT, NonEmptyList, StateT}
import cats.syntax.applicative.*
import cats.syntax.either.*
import cats.syntax.eq.*
import cats.syntax.option.*
import cats.{Eq, Monad}
import com.peknight.error.std.Error
import com.peknight.password.gen.charsets.accumulation.{CharsetAccumulation, ConsecutiveAccumulation, GenAccumulation}
import com.peknight.password.gen.charsets.interval.LengthInterval
import com.peknight.password.gen.charsets.option.{CharsetOption, Consecutive, GenOption}
import com.peknight.password.gen.charsets.validation.{checkBounded, checkCharsetOption, checkConsecutiveOption, checkLength}
import com.peknight.random.Random
import com.peknight.random.state.{between, nextIntBounded}
import com.peknight.validation.collection.iterableOnce.either.nonEmpty
import com.peknight.validation.collection.list.either.nonEmpty as listNonEmpty
import com.peknight.validation.spire.math.interval.either.nonNegative
import com.peknight.validation.traverse.either.traverse
import spire.math.Interval

object CharsetsGen:

  private[this] type EitherStateT[F[_], A] = EitherT[[S] =>> StateT[F, Random[F], S], Error, A]

  extension[A] (either: Either[Error, A])
    def liftE[F[_]: Monad]: EitherStateT[F, A] = EitherT(either.pure[[S] =>> StateT[F, Random[F], S]])
  end extension
  extension [F[_]: Monad, A] (state: StateT[F, Random[F], A])
    def liftS: EitherStateT[F, A] = EitherT(state.map(_.asRight))
  end extension

  def generate[F[_]: Monad, K: Eq, C <: Iterable[Char]](option: GenOption[K, C])
  : StateT[F, Random[F], Either[Error, String]] =
    StateT.get[F, Random[F]].liftS.flatMap { random => (
      for
      // 参数检查
        global <- checkLength(option.length).liftE
        consecutiveOption <- checkConsecutiveOption(option.consecutive).liftE
        _ <- nonNegative(option.retry, "retry").liftE
        charsets <- listNonEmpty(option.charsets.toList, "charsets").liftE
        // 各字符集长度区间检查
        charsetLengths <- traverse(charsets, "charsets") {
          case (k, charsetOption) => checkCharsetOption(charsetOption).map((k, _)).left.map(k *: _)
        }.liftE
        // 长度区间求和
        charsetSum = LengthInterval.sum(charsetLengths.map(_._2))
        // 求全局区间与和区间交集并检查存在上限
        global <- checkLength(global.intersect(charsetSum)).flatMap(checkBounded)
          .left.map((global, charsetSum) *: _).liftE
        // 生成
        result <- generate(global, charsetLengths, option.charsets, consecutiveOption, option.retry)
      yield result
    ).leftMap(random *: _) }.value

  private[this] def generate[F[_]: Monad, K: Eq, C <: Iterable[Char]](global: LengthInterval,
                                                                      charsetLengths: NonEmptyList[(K, LengthInterval)],
                                                                      charsets: Map[K, CharsetOption[C]],
                                                                      consecutiveOption: Option[Consecutive],
                                                                      retry: Int): EitherStateT[F, String] =
    // 全局区间上限
    val globalUpper = global.upperOption.getOrElse(Int.MaxValue)
    val result =
      for
      // 生成字符串长度
        length <-
          // 长度区间唯一则直接取值
          if global.lower == globalUpper then global.lower.pure[[A] =>> EitherStateT[F, A]]
          // 否则随机
          else between[F](global.lower, globalUpper + 1).liftS
        // 细化各字符集长度区间
        charsetLengthMap = calculateLengths(length, charsetLengths.toList.toMap)
        // 结构转换，生成初始值
        charsetAccumulations = charsets.map { case (k, charsetOption) =>
          (k, CharsetAccumulation(charsetOption.chars.toVector, charsetLengthMap(k), charsetOption.repeat,
            charsetOption.startsWith, charsetOption.endsWith))
        }
        // 逐个生成字符
        res <- generate(length, charsetAccumulations, consecutiveOption)
      yield res
    // 失败重试
    Monad[[A] =>> EitherStateT[F, A]].tailRecM(retry) { retry =>
      if retry <= 0 then result.map(_.asRight)
      else EitherT(result.fold(_ => (retry - 1).asLeft[String].asRight[Error], _.asRight[Int].asRight[Error]))
    }

  private[this] def generate[F[_]: Monad, K: Eq](length: Int, charsetAccumulations: Map[K, CharsetAccumulation],
                                                 consecutiveOption: Option[Consecutive]): EitherStateT[F, String] =
    Monad[[A] =>> EitherStateT[F, A]].tailRecM(GenAccumulation(length, List.empty, charsetAccumulations, None)) {
      case context @ GenAccumulation(remain, chars, charsetAccs, consecutiveAccOption) =>
        // 待生成长度为0，生成结束，返回结果
        if remain == 0 then chars.reverse.mkString.asRight.pure else
          // 过滤掉无法出现在下一位的字符、字符集
          val charMap = filterChars(charsetAccs, consecutiveOption, consecutiveAccOption, chars.isEmpty, remain == 1)
          for
          // 检查字符集不为空
            charMap <- nonEmpty(charMap, "charMap").left.map(context *: _).liftE
            // 随机取字符集
            index <- nextIndex(charMap.size)
            key = charMap.keys.toVector(index)
            charVector = charMap(key)
            // 随机取字符
            index <- nextIndex(charVector.size)
            ch = charVector(index)
          yield
            // 剩余字符数
            val nextRemain = remain - 1
            // 当前生成使用的字符集
            val charsetAcc = charsetAccs(key)
            // 细化各字符集长度区间
            val charsetLengthMap = calculateLengths(nextRemain,
              (charsetAccs - key).map((k, charsetAcc) => (k, charsetAcc.length)) + (key -> (charsetAcc.length - 1))
            )
            // 更新各字符集中间值
            val nextCharsetAccs = charsetAccs.foldLeft(charsetAccs) { case (accs, (k, acc)) =>
              // 当前字符集更新
              if k === key then accs + (k -> acc.copy(chars = nextChars(ch, charsetAcc), length = charsetLengthMap(key)))
              // 其它字符集仅更新长度区间
              else accs + (k -> acc.copy(length = charsetLengthMap(k)))
            }
            // 中间值更新
            GenAccumulation(nextRemain, ch :: chars, nextCharsetAccs,
              nextConsecutiveAccumulation(ch, consecutiveOption, consecutiveAccOption)
            ).asLeft
    }

  def allocate[F[_]: Monad, K](global: Interval[Int], elements: Map[K, Interval[Int]])
  : Either[Error, StateT[F, Random[F], Map[K, Int]]] =
    for
      global <- checkLength(global)
      elements <- listNonEmpty(elements.toList, "elements")
      elements <- traverse(elements, "elements") {
        case (k, length) => checkLength(length).map((k, _)).left.map(k *: _)
      }
      elementSum = LengthInterval.sum(elements.map(_._2))
      global <- checkLength(global.intersect(elementSum)).flatMap(checkBounded)
        .left.map((global, elementSum) *: _)
    yield
      Monad[[A] =>> StateT[F, Random[F], A]].tailRecM((elements.toList, global, Map.empty[K, Int])) {
        case (remain, global, map) =>
          if remain.isEmpty then map.asRight.pure else
            for
              index <- if remain.length == 1 then StateT.pure[F, Random[F], Int](0) else nextIntBounded[F](remain.length)
              (left, right) = remain.splitAt(index)
              (k, current) = right.head
              nextRemain = left ::: right.tail
              remainLength = LengthInterval.sum(nextRemain.map(_._2))
              currentLength = calculateLength(current, global, remainLength)
              currentUpper = currentLength.upperOption.getOrElse(Int.MaxValue)
              len <-
                if currentLength.lower == currentUpper then StateT.pure[F, Random[F], Int](currentLength.lower)
                else between(currentLength.lower, currentUpper + 1)
            yield (nextRemain, calculateRemainLength(len, global, remainLength), map + (k -> len)).asLeft
      }

  private[this] def calculateLength(current: LengthInterval, global: LengthInterval, remain: LengthInterval)
  : LengthInterval =
    val lower = remain.upperOption match
      case Some(remainUpper) if remainUpper < global.lower => current.lower max (global.lower - remainUpper)
      case _ => current.lower
    val upperOption = (current.upperOption, global.upperOption) match
      case (Some(currentUpper), Some(globalUpper)) => (currentUpper min (globalUpper - remain.lower)).some
      case (Some(currentUpper), None) => currentUpper.some
      case (None, Some(globalUpper)) => (globalUpper - remain.lower).some
      case _ => none[Int]
    LengthInterval(lower, upperOption)

  private[this] def calculateLengths[K](remain: Int, elements: Map[K, LengthInterval]): Map[K, LengthInterval] =
    val global = LengthInterval.point(remain)
    elements.foldLeft(elements) { case (elements, (k, length)) =>
      elements + (k -> calculateLength(length, global, LengthInterval.sum((elements - k).values.toList)))
    }

  private[this] def calculateRemainLength(length: Int, global: LengthInterval, remain: LengthInterval): LengthInterval =
    val lower = (global.lower - length) max remain.lower
    val upperOption = (remain.upperOption, global.upperOption) match
      case (Some(remainUpper), Some(globalUpper)) => ((globalUpper - length) min remainUpper).some
      case (Some(remainUpper), None) => remainUpper.some
      case (None, Some(globalUpper)) => (globalUpper - length).some
      case _ => none[Int]
    LengthInterval(lower, upperOption)

  private[this] def nextIndex[F[_]: Monad](size: Int): EitherStateT[F, Int] =
    if size == 1 then 0.pure[[A] =>> EitherStateT[F, A]]
    else nextIntBounded[F](size).liftS

  private[this] def filterChars[K](charsetAccs: Map[K, CharsetAccumulation], consecutiveOption: Option[Consecutive],
                                   consecutiveAccOption: Option[ConsecutiveAccumulation], start: Boolean, end: Boolean)
  : Map[K, Vector[Char]] =
    charsetAccs.filter((_, charsetAcc) => charsetAcc.length.upperOption.forall(_ > 0) &&
        (!start || charsetAcc.startsWith) &&
        (!end || charsetAcc.endsWith))
      .map((k, charsetAcc) => (k, consecutiveOption
        .flatMap(consecutive => consecutiveAccOption
          .filter(acc => consecutive.max == acc.length)
          .map(acc => charsetAcc.chars.filter(ch => !isConsecutive(ch, consecutive, acc))))
        .getOrElse(charsetAcc.chars)
      ))
      .filter((_, chars) => chars.nonEmpty)

  private[this] def nextChars(ch: Char, charsetAcc: CharsetAccumulation): Vector[Char] =
    if charsetAcc.repeat then charsetAcc.chars else
      charsetAcc.chars.foldLeft((Vector.empty[Char], false)) { case ((acc, flag), c) =>
        if flag then
          (acc :+ c, true)
        else if c == ch then (acc, true) else (acc :+ c, false)
      }._1

  private[this] def calculateStep(ch: Char, consecutive: Consecutive, consecutiveAcc: ConsecutiveAccumulation): Int =
    if consecutive.caseSensitive then ch - consecutiveAcc.current
    else ch.toLower - consecutiveAcc.current.toLower

  private[this] def isConsecutive(ch: Char, consecutive: Consecutive, consecutiveAcc: ConsecutiveAccumulation): Boolean =
    if ch.isLetterOrDigit then
      val step = calculateStep(ch, consecutive, consecutiveAcc)
      consecutiveAcc.step match
        case Some(accStep) => accStep == step
        case _ => step.abs <= consecutive.step
    else false

  private[this] def nextConsecutiveAccumulation(ch: Char, consecutiveOption: Option[Consecutive],
                                                consecutiveAccOption: Option[ConsecutiveAccumulation])
  : Option[ConsecutiveAccumulation] =
    if !ch.isLetterOrDigit then None else
      (consecutiveOption, consecutiveAccOption) match
        case (None, _) => None
        case (Some(_), None) => Some(ConsecutiveAccumulation(ch, 1, None))
        case (Some(consecutive), Some(consecutiveAcc)) =>
          val step = calculateStep(ch, consecutive, consecutiveAcc)
          consecutiveAcc.step match
            case Some(accStep) if step.abs <= consecutive.step =>
              if accStep == step then Some(ConsecutiveAccumulation(ch, consecutiveAcc.length + 1, Some(step)))
              else Some(ConsecutiveAccumulation(ch, 2, Some(step)))
            case _ => Some(ConsecutiveAccumulation(ch, 1, None))
  end nextConsecutiveAccumulation
end CharsetsGen
