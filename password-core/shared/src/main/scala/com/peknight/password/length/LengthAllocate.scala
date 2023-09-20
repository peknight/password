package com.peknight.password.length

import cats.Monad
import cats.data.{NonEmptyList, StateT}
import cats.syntax.applicative.*
import cats.syntax.either.*
import cats.syntax.option.*
import com.peknight.error.spire.math.interval.{BoundEmptyError, UnboundError}
import com.peknight.error.std.Error
import com.peknight.password.interval.LengthInterval
import com.peknight.random.Random
import com.peknight.random.state.{between, nextIntBounded}
import com.peknight.spire.ext.syntax.bound.get
import com.peknight.validation.collection.list.either.nonEmpty
import com.peknight.validation.spire.math.interval.either.{atOrAbove, nonNegative}
import com.peknight.validation.traverse.either.traverse
import spire.math.Interval
import spire.math.interval.{Bound, EmptyBound, Unbound, ValueBound}

object LengthAllocate:
  def allocate[F[_] : Monad, K](global: Interval[Int], elements: Map[K, Interval[Int]])
  : Either[Error, StateT[F, Random[F], Map[K, Int]]] =
    for
      global <- checkLength(global)
      elements <- nonEmpty(elements.toList, "elements")
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

  def checkLength(length: Interval[Int]): Either[Error, LengthInterval] =
    val res =
      for
        lower <- checkLowerBound(length.lowerBound)
        upperOption <- checkUpperBound(length.upperBound, lower)
      yield LengthInterval(lower, upperOption)
    res.left.map(length *: _)

  def checkLength(length: LengthInterval): Either[Error, LengthInterval] =
    val res =
      for
        lower <- nonNegative(length.lower, "lowerBound")
        upperOption <- traverse(length.upperOption, "upperBoundOption")(
          upper => atOrAbove(upper, lower, "upperBound")
        )
      yield length
    res.left.map(length *: _)

  private[this] def checkBounded(length: LengthInterval): Either[Error, LengthInterval] =
    length.upperOption.fold(UnboundError("upperBound").asLeft[LengthInterval]) {
      upper => length.asRight[Error]
    }
end LengthAllocate
