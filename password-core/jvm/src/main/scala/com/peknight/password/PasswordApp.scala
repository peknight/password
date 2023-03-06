package com.peknight.password

import cats.data.*
import cats.syntax.applicative.*
import cats.syntax.either.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import cats.syntax.validated.*
import cats.syntax.writer.*
import cats.{FlatMap, Functor, Id, Monad, Order, Semigroup}
import com.peknight.cats.ext.monad.transformer.writer.WriterIdT
import com.peknight.error.collection.CollectionEmptyError
import com.peknight.error.spire.math.IntervalEmptyError
import com.peknight.error.spire.math.interval.{BoundEmptyError, UnboundError}
import com.peknight.error.{Error, UndefinedError}
import com.peknight.random.Random
import com.peknight.random.id.Random as IdRandom
import com.peknight.random.state.*
import com.peknight.spire.ext.syntax.bound.get
import com.peknight.validation.collection.list.either.*
import com.peknight.validation.spire.math.interval.either.*
import spire.math.*
import spire.math.interval.*

import scala.collection.BuildFrom
import scala.compiletime.{constValue, erasedValue}

object PasswordApp extends App:

  trait CharGen[A]:
    def next[F[_] : Monad](a: A): StateT[F, Random[F], Char]
  end CharGen

  object CharGen:
    def apply[A](using gen: CharGen[A]): CharGen[A] = gen
  end CharGen

  given CharGen[String] with
    def next[F[_] : Monad](a: String): StateT[F, Random[F], Char] = nextIntBounded(a.length).map(a.charAt)
  end given

  extension [A : CharGen] (a: A)
    def next[F[_] : Monad]: StateT[F, Random[F], Char] = CharGen[A].next(a)
  end extension

  case class CharUnit[A : CharGen](a: A, length: Interval[Int])


  case class CharUnitState[F[_], A](random: Random[F], a: A, consecutive: Int, step: Int)


  println(lengths[Id, Int](Interval.above(100), Map(
    1 -> Interval.closed(50, 60),
    2 -> Interval.atOrBelow(50),
    3 -> Interval.closed(2, 10)
  )).map(_.runA(IdRandom(System.currentTimeMillis()))))

  // -----------------------------

  type LengthInterval = (Int, Option[Int])
  type BoundedLengthInterval = (Int, Int)

  def lengths[F[_] : Monad, K : Order](length: Interval[Int], elementIntervalMap: Map[K, Interval[Int]])
  : Either[Error, StateT[F, Random[F], Map[K, Int]]] =
    for
      lengthInterval <- checkLength(length)
      elementIntervals <- nonEmpty(elementIntervalMap.toList, "elements")
      elementLengthIntervals <- elementIntervals.traverse {
        case (k, interval) => checkLength(interval).map((k, _)).toValidated
      }.toEither
      globalLengthInterval <- intersect(lengthInterval, sum(elementLengthIntervals.map(_._2)), "intersect interval")
    yield
      Monad[[A] =>> StateT[F, Random[F], A]].tailRecM(
        (elementLengthIntervals.toList, globalLengthInterval, Map.empty[K, Int])
      ) { case (remain, global, map) =>
        if remain.isEmpty then map.asRight.pure else
          for
            index <- nextIntBounded[F](remain.length)
            (left, right) = remain.splitAt(index)
            (k, current) = right.head
            nextRemain = left ::: right.tail
            remainOption = sum(nextRemain.map(_._2))
            currentLengthInterval = calculateLengthInterval(current, global, remainOption)
            len <- between(currentLengthInterval._1, currentLengthInterval._2 + 1)
          yield (nextRemain, remainLengthInterval(len, global, remainOption), map + (k -> len)).asLeft
      }

  def checkLowerBound(lowerBound: Bound[Int]): Either[Error, Int] =
    val label = "lower bound"
    lowerBound match
      case bound: ValueBound[Int] => nonNegative(bound.get(true), label)
      case Unbound() => 0.asRight[Error]
      case EmptyBound() => BoundEmptyError(label).asLeft[Int]

  def checkUpperBound(upperBound: Bound[Int], lower: Int): Either[Error, Option[Int]] =
    val label = "upper bound"
    upperBound match
      case bound: ValueBound[Int] => atOrAbove(bound.get(false), label, lower).map(_.some)
      case Unbound() => none[Int].asRight[Error]
      case EmptyBound() => BoundEmptyError(label).asLeft[Option[Int]]

  def checkLength(length: Interval[Int]): Either[Error, LengthInterval] =
    for
      lower <- checkLowerBound(length.lowerBound)
      upperOption <- checkUpperBound(length.upperBound, lower)
    yield (lower, upperOption)

  def intersect(i: LengthInterval, o: LengthInterval, label: => String): Either[Error, BoundedLengthInterval] =
    val (iLower, iUpperOption) = i
    val (oLower, oUpperOption) = o
    val lower = iLower max oLower
    (iUpperOption, oUpperOption) match
      case (Some(iUpper), Some(oUpper)) =>
        val upper = iUpper min oUpper
        if lower <= upper then (lower, upper).asRight[Error]
        else IntervalEmptyError(label).asLeft[BoundedLengthInterval]
      case (Some(iUpper), _) =>
        if lower <= iUpper then (lower, iUpper).asRight[Error]
        else IntervalEmptyError(label).asLeft[BoundedLengthInterval]
      case (_, Some(oUpper)) => (lower, oUpper).asRight[Error]
      case _ => UnboundError(label).asLeft[BoundedLengthInterval]
  end intersect

  def sum(head: LengthInterval, tail: List[LengthInterval]): LengthInterval = tail.foldLeft(head) {
    case ((accLower, accUpperOption), (lower, upperOption)) =>
      (accLower + lower, accUpperOption.flatMap(accUpper => upperOption.map(accUpper + _)))
  }

  def sum(list: NonEmptyList[LengthInterval]): LengthInterval = sum(list.head, list.tail)

  def sum(list: List[LengthInterval]): Option[LengthInterval] = list match
    case head :: tail => sum(head, tail).some
    case _ => none[LengthInterval]

  def calculateLengthInterval(current: LengthInterval, global: BoundedLengthInterval, remainOption: Option[LengthInterval])
  : BoundedLengthInterval =
    val (currentLower, currentUpperOption) = current
    val (globalLower, globalUpper) = global
    val (remainLower, remainUpperOption) = remainOption match
      case Some((rLower, rUpperOption)) => (rLower, rUpperOption)
      case None => (0, 0.some)
    val lower = remainUpperOption match
      case Some(remainUpper) if remainUpper < globalLower => currentLower max (globalLower - remainUpper)
      case _ => currentLower
    val upper = currentUpperOption match
      case Some(currentUpper) => currentUpper min (globalUpper - remainLower)
      case _ => globalUpper - remainLower
    (lower, upper)

  def remainLengthInterval(length: Int, global: BoundedLengthInterval, remainOption: Option[LengthInterval])
  : BoundedLengthInterval =
    remainOption match
      case None => (0, 0)
      case Some(remainLower, remainUpperOption) =>
        val (globalLower, globalUpper) = global
        val lower = (globalLower - length) max remainLower
        val upper = remainUpperOption match
          case Some(remainUpper) => (globalUpper - length) min remainUpper
          case _ => globalUpper - length
        (lower, upper)
  end remainLengthInterval
