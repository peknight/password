package com.peknight.password

import cats.data.*
import cats.syntax.applicative.*
import cats.syntax.traverse.*
import cats.syntax.either.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import cats.syntax.validated.*
import cats.syntax.writer.*
import cats.{FlatMap, Functor, Monad, Semigroup}
import com.peknight.cats.ext.monad.transformer.writer.WriterIdT
import com.peknight.error.spire.math.interval.BoundEmptyError
import com.peknight.error.{Error, UndefinedError}
import com.peknight.random.Random
import com.peknight.random.id.Random as IdRandom
import com.peknight.random.state.*
import com.peknight.spire.ext.syntax.bound.get
import com.peknight.validation.spire.math.interval.id.either.IntervalValidation.*
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

  def toPair(len: Interval[Int]): (Int, Option[Int]) =
    val lower = len.lowerBound match
      case Closed(a) => a max 0
      case Open(a) => (a + 1) max 0
      case _ => 0
    len.upperBound match
      case Unbound() => (lower, None)
      case Closed(a) if lower <= a => (lower, Some(a))
      case Open(a) if lower <= a - 1 => (lower, Some(a - 1))
      case _ => (0, Some(0))
  end toPair

  extension (self: (Int, Option[Int]))
    def +(that: (Int, Option[Int])): (Int, Option[Int]) =
      val upperOption =
        for
          x <- self._2
          y <- that._2
        yield x + y
      (self._1 + that._1, upperOption)
  end extension

  def toInterval(pair: (Int, Option[Int])): Interval[Int] =
    val (lower, upperOption) = pair
    upperOption match
      case Some(upper) => Interval.closed(lower, upper)
      case _ => Interval.atOrAbove(lower)

  case class CharUnit[A : CharGen](a: A, length: Interval[Int]):
    def gen[F[_] : Monad](max: Int): StateT[F, Random[F], String] =
      val (lower, upperOption) = toPair(length)
      for
        len <- between(lower, upperOption.getOrElse(lower max max) + 1)
        chars <- List.fill(len)(a.next[F]).sequence
      yield chars.mkString

  case class PasswordGen[A : CharGen](units: NonEmptyList[CharUnit[A]], length: Interval[Int]):
    val unitLength = units.tail.foldLeft(toPair(units.head.length)) {
      (acc, unit) => acc + toPair(unit.length)
    }
    val (unitLower, unitUpperOption) = unitLength
    val (lower, upperOption) = toPair(toInterval(unitLength).intersect(length))
    val max = 128
    val unitsV = units.toNev.toVector

    def unitLength[F[_] : Monad, A : CharGen](units: Vector[CharUnit[A]], buffer: Int): StateT[F, Random[F], Map[CharUnit[A], Int]] =
      Monad[[A] =>> StateT[F, Random[F], A]].tailRecM((units, buffer, Map.empty[CharUnit[A], Int])) {
        case (units, buffer, map) =>
          if units.isEmpty then Monad[[A] =>> StateT[F, Random[F], A]].pure(Right(map))
          else
            for
              index <- nextIntBounded[F](units.size)
              (left, right) = units.splitAt(index)
              unit = right.head
              (lower, upperOption) = toPair(unit.length)
              u = lower + buffer
              u2 = upperOption.map(_ min u).getOrElse(u)
              l <- between(lower, u2 + 1)
            yield Left(left ++ right.tail, buffer - (l - lower), map.+(unit -> l))
      }








  val list: List[Interval[Int]] = List(Interval.atOrAbove(8), Interval.atOrBelow(10), Interval.closed(7, 13))

  println(CharUnit("abcdefgh", Interval.closed(6, 20)).gen(128).runA(IdRandom(System.currentTimeMillis())))

  case class CharUnitState[F[_], A](random: Random[F], a: A, consecutive: Int, step: Int)
  //def next[F[_] : Monad](a: String, consecutive: Option[Int], replace: Boolean): StateT[F, CharUnitState[F, String], Char] = StateT { state =>
  //}


  println(Vector(1, 2, 3, 4).splitAt(0))
  println(Vector(1, 2, 3, 4).splitAt(1))
  println(Vector(1, 2, 3, 4).splitAt(2))
  println(Vector(1, 2, 3, 4).splitAt(3))
  println(Vector(1, 2, 3, 4).splitAt(4))
  println(Vector(1, 2, 3, 4).splitAt(5))


  // -----------------------------

  /**
   * 最大长度剩余量
   * 总长度10
   * 0-5，0-2，0-3，0-1
   */
  def lengths[F[_] : Monad, K](length: Interval[Int], elements: Map[K, Interval[Int]])
  : ValidatedNel[Error, StateT[F, Random[F], Map[K, Int]]] =
    elements.toList.traverse { case (k, interval) => checkLength(interval).map((k, _)).toValidatedNel } map { list =>
      val map: Map[K, (Int, Option[Int])] = list.toMap


    }

    ???




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

  type LengthInterval = (Int, Option[Int])

  def checkLength(length: Interval[Int]): Either[Error, LengthInterval] =
    for
      lower <- checkLowerBound(length.lowerBound)
      upperOption <- checkUpperBound(length.upperBound, lower)
    yield (lower, upperOption)
