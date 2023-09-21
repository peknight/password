package com.peknight.password.gen.charsets

import cats.syntax.either.*
import cats.syntax.option.*
import com.peknight.error.spire.math.interval.{BoundEmptyError, UnboundError}
import com.peknight.error.std.Error
import com.peknight.password.gen.charsets.interval.LengthInterval
import com.peknight.password.gen.charsets.option.{CharsetOption, Consecutive}
import com.peknight.spire.ext.syntax.bound.get
import com.peknight.validation.collection.iterableOnce.either.nonEmpty
import com.peknight.validation.spire.math.interval.either.{atOrAbove, nonNegative, positive}
import com.peknight.validation.traverse.either.traverse
import spire.math.Interval
import spire.math.interval.{Bound, EmptyBound, Unbound, ValueBound}

object validation:
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

  def checkLength(length: Interval[Int]): Either[Error, LengthInterval] = {
    for
      lower <- checkLowerBound(length.lowerBound)
      upperOption <- checkUpperBound(length.upperBound, lower)
    yield
      LengthInterval(lower, upperOption)
  }.left.map(length *: _)

  def checkLength(length: LengthInterval): Either[Error, LengthInterval] = {
    for
      lower <- nonNegative(length.lower, "lowerBound")
      _ <- traverse(length.upperOption, "upperBoundOption")(
        upper => atOrAbove(upper, lower, "upperBound")
      )
    yield
      length
  }.left.map(length *: _)

  def checkBounded(length: LengthInterval): Either[Error, LengthInterval] =
    length.upperOption.fold((length *: UnboundError("upperBound")).asLeft[LengthInterval])(_ => length.asRight[Error])

  def checkCharsetOption[C <: Iterable[Char]](charsetOption: CharsetOption[C]): Either[Error, LengthInterval] =
    for
      length <- checkLength(charsetOption.length)
      chars <- nonEmpty(charsetOption.chars, "chars")
      length <-
        if charsetOption.repeat then
          length.asRight[Error]
        else
          val charLength = LengthInterval.atOrBelow(chars.size)
          checkLength(length.intersect(charLength)).left.map((length, charLength) *: _)
    yield
      length

  def checkConsecutiveOption(consecutive: Option[Consecutive]): Either[Error, Option[Consecutive]] =
    traverse(consecutive, "consecutive") { consecutive =>
      for
        _ <- positive(consecutive.max, "max")
        _ <- nonNegative(consecutive.step, "step")
      yield consecutive
    }
end validation
