package com.peknight.password.length

import cats.Monad
import cats.data.{NonEmptyList, StateT}
import cats.syntax.applicative.*
import cats.syntax.either.*
import cats.syntax.option.*
import com.peknight.error.spire.math.IntervalEmptyError
import com.peknight.error.spire.math.interval.{BoundEmptyError, UnboundError}
import com.peknight.error.std.Error
import com.peknight.password.domain.{BoundedLengthInterval, LengthInterval}
import com.peknight.random.Random
import com.peknight.random.state.{between, nextIntBounded}
import com.peknight.spire.ext.syntax.bound.get
import com.peknight.validation.collection.list.either.nonEmpty
import com.peknight.validation.spire.math.interval.either.{atOrAbove, nonNegative}
import com.peknight.validation.traverse.either.traverse
import spire.math.Interval
import spire.math.interval.{Bound, EmptyBound, Unbound, ValueBound}

object LengthAllocate:
  def allocate[F[_] : Monad, K](length: Interval[Int], elementLengths: Map[K, Interval[Int]])
  : Either[Error, StateT[F, Random[F], Map[K, Int]]] =
    for
      lengthInterval <- checkLength(length)
      elementIntervals <- nonEmpty(elementLengths.toList, "elementLengths")
      elementLengthIntervals <- traverse(elementIntervals, "elementIntervals") {
        case (k, interval) => checkLength(interval).map((k, _))
      }
      globalLengthInterval <- intersect(lengthInterval, sum(elementLengthIntervals.map(_._2)))
    yield
      Monad[[A] =>> StateT[F, Random[F], A]].tailRecM(
        (elementLengthIntervals.toList, globalLengthInterval, Map.empty[K, Int])
      ) { case (remain, global, map) =>
        if remain.isEmpty then map.asRight.pure else
          for
            index <- if remain.length == 1 then StateT.pure[F, Random[F], Int](0) else nextIntBounded[F](remain.length)
            (left, right) = remain.splitAt(index)
            (k, current) = right.head
            nextRemain = left ::: right.tail
            remainOption = sum(nextRemain.map(_._2))
            currentLengthInterval = calculateLengthInterval(current, global, remainOption)
            len <-
              if currentLengthInterval.lower == currentLengthInterval.upper then
                StateT.pure[F, Random[F], Int](currentLengthInterval.lower)
              else
                between(currentLengthInterval.lower, currentLengthInterval.upper + 1)
          yield (nextRemain, remainLengthInterval(len, global, remainOption), map + (k -> len)).asLeft
      }

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

  def intersect(i: LengthInterval, o: LengthInterval): Either[Error, BoundedLengthInterval] =
    val lower = i.lower max o.lower
    val upperOption =
      (i.upper, o.upper) match
        case (Some(iUpper), Some(oUpper)) => (iUpper min oUpper).some
        case (Some(iUpper), _) => iUpper.some
        case (_, Some(oUpper)) => oUpper.some
        case _ => none[Int]
    upperOption.fold(UnboundError("intersectUpperBound").asLeft[BoundedLengthInterval])(upper =>
      if lower <= upper then BoundedLengthInterval(lower, upper).asRight[Error]
      else IntervalEmptyError("intersectInterval").asLeft[BoundedLengthInterval]
    ).left.map((i, o) *: _)
  end intersect

  private[this] def sum(head: LengthInterval, tail: List[LengthInterval]): LengthInterval =
    tail.foldLeft(head)((acc, current) => LengthInterval(
      acc.lower + current.lower,
      acc.upper.flatMap(accUpper => current.upper.map(accUpper + _))
    ))

  private[this] def sum(list: NonEmptyList[LengthInterval]): LengthInterval = sum(list.head, list.tail)

  private[this] def sum(list: List[LengthInterval]): Option[LengthInterval] =
    list match
      case head :: tail => sum(head, tail).some
      case _ => none[LengthInterval]

  private[this] def calculateLengthInterval(current: LengthInterval, global: BoundedLengthInterval,
                                            remainOption: Option[LengthInterval]): BoundedLengthInterval =
    val remain = remainOption.getOrElse(LengthInterval(0, 0.some))
    val lower = remain.upper match
      case Some(remainUpper) if remainUpper < global.lower => current.lower max (global.lower - remainUpper)
      case _ => current.lower
    val upper = current.upper match
      case Some(currentUpper) => currentUpper min (global.upper - remain.lower)
      case _ => global.upper - remain.lower
    BoundedLengthInterval(lower, upper)

  private[this] def remainLengthInterval(length: Int, global: BoundedLengthInterval,
                                         remainOption: Option[LengthInterval]): BoundedLengthInterval =
    remainOption.fold(BoundedLengthInterval(0, 0)){ remain =>
      val lower = (global.lower - length) max remain.lower
      val upper = remain.upper match
        case Some(remainUpper) => (global.upper - length) min remainUpper
        case _ => global.upper - length
      BoundedLengthInterval(lower, upper)
    }
  end remainLengthInterval
end LengthAllocate
