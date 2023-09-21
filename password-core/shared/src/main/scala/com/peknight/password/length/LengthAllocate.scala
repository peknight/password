package com.peknight.password.length

import cats.data.{EitherT, NonEmptyList, StateT}
import cats.syntax.applicative.*
import cats.syntax.either.*
import cats.syntax.eq.*
import cats.syntax.functor.*
import cats.syntax.option.*
import cats.{Eq, Monad}
import com.peknight.error.spire.math.interval.{BoundEmptyError, UnboundError}
import com.peknight.error.std.Error
import com.peknight.password.interval.LengthInterval
import com.peknight.random.Random
import com.peknight.random.state.{between, nextIntBounded}
import com.peknight.spire.ext.syntax.bound.get
import com.peknight.validation.collection.iterableOnce.either.nonEmpty
import com.peknight.validation.collection.list.either.nonEmpty as listNonEmpty
import com.peknight.validation.spire.math.interval.either.{atOrAbove, nonNegative, positive}
import com.peknight.validation.traverse.either.traverse
import spire.math.Interval
import spire.math.interval.{Bound, EmptyBound, Unbound, ValueBound}

object LengthAllocate:

  case class CharsetOption[+C <: Iterable[Char]](chars: C, length: Interval[Int], repeat: Boolean)
  case class Consecutive(max: Int, step: Int)
  case class StringGenOption[K, C <: Iterable[Char]](length: Interval[Int], charsets: Map[K, CharsetOption[C]],
                                                     consecutive: Option[Consecutive], retry: Int = 2)
  case class CharsetAccumulation(chars: Vector[Char], length: LengthInterval, repeat: Boolean)
  case class ConsecutiveAccumulation(current: Char, length: Int, step: Option[Int])
  case class StringGenAccumulation[K](remain: Int, chars: List[Char], charsetAccumulations: Map[K, CharsetAccumulation],
                                      consecutiveAccumulation: Option[ConsecutiveAccumulation])

  type EitherStateT[F[_], A] = EitherT[[S] =>> StateT[F, Random[F], S], Error, A]

  extension[A] (either: Either[Error, A])
    def liftE[F[_]: Monad]: EitherStateT[F, A] = EitherT(either.pure[[S] =>> StateT[F, Random[F], S]])
  end extension
  extension [F[_]: Monad, A] (state: StateT[F, Random[F], A])
    def liftS: EitherStateT[F, A] = EitherT(state.map(_.asRight))
  end extension

  def generate[F[_]: Monad, K: Eq, C <: Iterable[Char]](option: StringGenOption[K, C])
  : StateT[F, Random[F], Either[Error, String]] =
    StateT.get[F, Random[F]].liftS.flatMap { random => (
      for
        // 参数检查
        globalLength <- checkLength(option.length).liftE
        consecutiveOption <- checkConsecutiveOption(option.consecutive).liftE
        _ <- nonNegative(option.retry, "retry").liftE
        charsets <- listNonEmpty(option.charsets.toList, "charsets").liftE
        // 各字符集长度区间检查
        charsetLengths <- traverse(charsets, "charsets") {
          case (k, charsetOption) => checkCharsetOption(charsetOption).map((k, _)).left.map(k *: _)
        }.liftE
        // 长度区间求和
        elementSum = sum(charsetLengths.map(_._2))
        // 求全局区间与和区间交集并检查存在上限
        globalLength <- checkLength(intersect(globalLength, elementSum)).flatMap(checkBounded)
          .left.map((globalLength, elementSum) *: _).liftE
        // 生成
        result <- generate(globalLength, charsetLengths, option.charsets, consecutiveOption, option.retry)
      yield result
    ).leftMap(random *: _)}.value

  private[this] def generate[F[_]: Monad, K: Eq, C <: Iterable[Char]](length: LengthInterval,
                                                                      charsetLengths: NonEmptyList[(K, LengthInterval)],
                                                                      charsets: Map[K, CharsetOption[C]],
                                                                      consecutiveOption: Option[Consecutive],
                                                                      retry: Int): EitherStateT[F, String] =
    // 全局区间上限
    val globalUpper = length.upperOption.getOrElse(Int.MaxValue)
    val result =
      for
        // 生成字符串长度
        length <-
          // 长度区间唯一则直接取值
          if length.lower == globalUpper then length.lower.pure[[A] =>> EitherStateT[F, A]]
          // 否则随机
          else between[F](length.lower, globalUpper + 1).liftS
        // 细化各字符集长度区间
        charsetLengthMap = calculateIntervals(length, charsetLengths.toList.toMap)
        // 结构转换，生成初始值
        charsetAccumulations = charsets.map { case (k, charsetOption) =>
          (k, CharsetAccumulation(charsetOption.chars.toVector, charsetLengthMap(k), charsetOption.repeat))
        }
        // 逐个生成字符
        res <- generate(length, charsetAccumulations, consecutiveOption)
      yield res
    Monad[[A] =>> EitherStateT[F, A]].tailRecM(retry) { retry =>
      if retry <= 0 then result.map(_.asRight)
      else EitherT(result.fold(error => (retry - 1).asLeft[String].asRight[Error], s => s.asRight[Int].asRight[Error]))
    }

  private[this] def generate[F[_]: Monad, K: Eq](length: Int, charsetAccumulations: Map[K, CharsetAccumulation],
                                                 consecutiveOption: Option[Consecutive]): EitherStateT[F, String] =
    Monad[[A] =>> EitherStateT[F, A]].tailRecM(StringGenAccumulation(length, List.empty, charsetAccumulations, None)) {
      case context @ StringGenAccumulation(remain, chars, charsetAccs, consecutiveAccOption) =>
        // 待生成长度为0，生成结束，返回结果
        if remain == 0 then chars.reverse.mkString.asRight.pure else
          // 过滤掉无法出现在下一位的字符、字符集
          val charMap = filterChars(charsetAccs, consecutiveOption, consecutiveAccOption)
          for
          // 检查字符集不为空
            charMap <- nonEmpty(charMap, "charMap").left.map(context *: _).liftE
            // 随机取字符集
            index <- nextIntBounded[F](charMap.size).liftS
            k = charMap.keys.toVector(index)
            charVector = charMap(k)
            // 随机取字符
            index <- nextIntBounded[F](charVector.size).liftS
            ch = charVector(index)
          yield
            // 剩余字符数
            val nextRemain = remain - 1
            // 当前生成使用的字符集
            val charsetAcc = charsetAccs(k)
            val newMap = (charsetAccs - k).map((k, charsetAcc) => (k, charsetAcc.length)) + (k -> (charsetAcc.length - 1))
            // 细化各字符集长度区间
            val charsetLengthMap = calculateIntervals(nextRemain, newMap)
            // 更新各字符集中间值
            val nextCharsetAccs = charsetAccs.foldLeft(charsetAccs) { case (accs, (key, acc)) =>
              // 当前字符集更新
              if key === k then accs + (key -> acc.copy(chars = nextChars(ch, charsetAcc), length = charsetLengthMap(k)))
              // 其它字符集仅更新长度区间
              else accs + (key -> acc.copy(length = charsetLengthMap(key)))
            }
            // 中间值更新
            StringGenAccumulation(nextRemain, ch :: chars, nextCharsetAccs,
              nextConsecutiveAccumulation(ch, consecutiveOption, consecutiveAccOption)).asLeft
    }

  def allocate[F[_]: Monad, K](global: Interval[Int], elements: Map[K, Interval[Int]])
  : Either[Error, StateT[F, Random[F], Map[K, Int]]] =
    for
      global <- checkLength(global)
      elements <- listNonEmpty(elements.toList, "elements")
      elements <- traverse(elements, "elements") {
        case (k, interval) => checkLength(interval).map((k, _)).left.map(k *: _)
      }
      elementSum = sum(elements.map(_._2))
      global <- checkLength(intersect(global, elementSum)).flatMap(checkBounded)
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
              remainOption = sum(nextRemain.map(_._2))
              currentInterval = calculateInterval(current, global, remainOption)
              currentUpper = currentInterval.upperOption.getOrElse(Int.MaxValue)
              len <-
                if currentInterval.lower == currentUpper then StateT.pure[F, Random[F], Int](currentInterval.lower)
                else between(currentInterval.lower, currentUpper + 1)
            yield (nextRemain, remainInterval(len, global, remainOption), map + (k -> len)).asLeft
      }

  private[this] def sum(head: LengthInterval, tail: List[LengthInterval]): LengthInterval =
    tail.foldLeft(head)((acc, current) => LengthInterval(
      acc.lower + current.lower,
      acc.upperOption.flatMap(accUpper => current.upperOption.map(accUpper + _))
    ))

  private[this] def sum(list: NonEmptyList[LengthInterval]): LengthInterval = sum(list.head, list.tail)

  private[this] def sum(list: List[LengthInterval]): Option[LengthInterval] =
    list match
      case head :: tail => sum(head, tail).some
      case _ => none[LengthInterval]

  private[this] def intersect(i: LengthInterval, o: LengthInterval): LengthInterval =
    val lower = i.lower max o.lower
    val upperOption =
      (i.upperOption, o.upperOption) match
        case (Some(iUpper), Some(oUpper)) => (iUpper min oUpper).some
        case (Some(iUpper), _) => iUpper.some
        case (_, Some(oUpper)) => oUpper.some
        case _ => none[Int]
    LengthInterval(lower, upperOption)
  end intersect

  private[this] def calculateInterval(current: LengthInterval, global: LengthInterval,
                                      remainOption: Option[LengthInterval]): LengthInterval =
    val remain = remainOption.getOrElse(LengthInterval.empty)
    val lower = remain.upperOption match
      case Some(remainUpper) if remainUpper < global.lower => current.lower max (global.lower - remainUpper)
      case _ => current.lower
    val upperOption = (current.upperOption, global.upperOption) match
      case (Some(currentUpper), Some(globalUpper)) => (currentUpper min (globalUpper - remain.lower)).some
      case (Some(currentUpper), None) => currentUpper.some
      case (None, Some(globalUpper)) => (globalUpper - remain.lower).some
      case _ => none[Int]
    LengthInterval(lower, upperOption)

  private[this] def calculateIntervals[K](remain: Int, elements: Map[K, LengthInterval]): Map[K, LengthInterval] =
    val global = LengthInterval(remain, remain)
    elements.foldLeft(elements) { case (elements, (k, length)) =>
      elements + (k -> calculateInterval(length, global, sum((elements - k).values.toList)))
    }

  private[this] def remainInterval(length: Int, global: LengthInterval, remainOption: Option[LengthInterval])
  : LengthInterval =
    remainOption.fold(LengthInterval.empty) { remain =>
      val lower = (global.lower - length) max remain.lower
      val upperOption = (remain.upperOption, global.upperOption) match
        case (Some(remainUpper), Some(globalUpper)) => ((globalUpper - length) min remainUpper).some
        case (Some(remainUpper), None) => remainUpper.some
        case (None, Some(globalUpper)) => (globalUpper - length).some
        case _ => none[Int]
      LengthInterval(lower, upperOption)
    }
  end remainInterval

  private[this] def filterChars[K](charsetAccs: Map[K, CharsetAccumulation], consecutiveOption: Option[Consecutive],
                     consecutiveAccOption: Option[ConsecutiveAccumulation]): Map[K, Vector[Char]] =
    charsetAccs.filter((k, charsetAcc) => charsetAcc.length.upperOption.forall(_ > 0))
      .map((k, charsetAcc) => (k, consecutiveOption.flatMap(consecutive => consecutiveAccOption
        .filter(acc => consecutive.max == acc.length)
        .map(acc => charsetAcc.chars.filter(ch => !isConsecutive(ch, consecutive, acc)))
      ).getOrElse(charsetAcc.chars)))
      .filter((k, chars) => chars.nonEmpty)

  private[this] def nextChars(ch: Char, charsetAcc: CharsetAccumulation): Vector[Char] =
    if charsetAcc.repeat then charsetAcc.chars else
      charsetAcc.chars.foldLeft((Vector.empty[Char], false)) { case ((acc, flag), c) =>
        if flag then
          (acc :+ c, true)
        else if c == ch then (acc, true) else (acc :+ c, false)
      }._1

  private[this] def isConsecutive(ch: Char, consecutive: Consecutive, consecutiveAcc: ConsecutiveAccumulation): Boolean =
    if ch.isLetterOrDigit then
      val step = ch.toLower - consecutiveAcc.current.toLower
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
        case (Some(consecutive), None) => Some(ConsecutiveAccumulation(ch, 1, None))
        case (Some(consecutive), Some(consecutiveAcc)) =>
          val step = ch.toLower - consecutiveAcc.current.toLower
          consecutiveAcc.step match
            case Some(accStep) if step.abs <= consecutive.step =>
              if accStep == step then Some(ConsecutiveAccumulation(ch, consecutiveAcc.length + 1, Some(step)))
              else Some(ConsecutiveAccumulation(ch, 2, Some(step)))
            case _ => Some(ConsecutiveAccumulation(ch, 1, None))
  end nextConsecutiveAccumulation

  private[this] def checkLowerBound(lowerBound: Bound[Int]): Either[Error, Int] =
    val label = "lowerBound"
    lowerBound match
      case bound: ValueBound[Int] => nonNegative(bound.get(true), label)
      case Unbound() => 0.asRight[Error]
      case EmptyBound() => BoundEmptyError(label).asLeft[Int]

  private[this] def checkUpperBound(upperBound: Bound[Int], lower: Int): Either[Error, Option[Int]] =
    val label = "upperBound"
    upperBound match
      case bound: ValueBound[Int] => atOrAbove(bound.get(false), lower, label).map(_.some)
      case Unbound() => none[Int].asRight[Error]
      case EmptyBound() => BoundEmptyError(label).asLeft[Option[Int]]

  private[this] def checkLength(length: Interval[Int]): Either[Error, LengthInterval] =
    val res =
      for
        lower <- checkLowerBound(length.lowerBound)
        upperOption <- checkUpperBound(length.upperBound, lower)
      yield LengthInterval(lower, upperOption)
    res.left.map(length *: _)

  private[this] def checkLength(length: LengthInterval): Either[Error, LengthInterval] =
    val res =
      for
        lower <- nonNegative(length.lower, "lowerBound")
        upperOption <- traverse(length.upperOption, "upperBoundOption")(
          upper => atOrAbove(upper, lower, "upperBound")
        )
      yield length
    res.left.map(length *: _)

  private[this] def checkBounded(length: LengthInterval): Either[Error, LengthInterval] =
    length.upperOption.fold(UnboundError("upperBound").asLeft[LengthInterval])(_ => length.asRight[Error])

  private[this] def checkCharsetOption[C <: Iterable[Char]](charsetOption: CharsetOption[C]): Either[Error, LengthInterval] =
    for
      length <- checkLength(charsetOption.length)
      chars <- listNonEmpty(charsetOption.chars.toList, "chars")
      charSize = chars.length
      length <-
        if charsetOption.repeat then
          length.asRight[Error]
        else
          val charLength = LengthInterval.atOrBelow(chars.length)
          checkLength(intersect(length, charLength)).left.map((length, charLength) *: _)
    yield
      length

  private[this] def checkConsecutiveOption(consecutive: Option[Consecutive]): Either[Error, Option[Consecutive]] =
    traverse(consecutive, "consecutive") { consecutive =>
      for
        _ <- positive(consecutive.max, "max")
        _ <- nonNegative(consecutive.step, "step")
      yield consecutive
    }

end LengthAllocate
