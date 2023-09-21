package com.peknight.password.gen.charsets.interval

import cats.data.NonEmptyList
import cats.syntax.option.*
import spire.math.Interval

sealed trait LengthInterval:
  def lower: Int
  def upperOption: Option[Int]

  def +(i: Int): LengthInterval = LengthInterval(
    if lower + i > 0 then lower + i else 0,
    upperOption.map(upper => if upper + i > 0 then upper + i else 0)
  )

  def -(i: Int): LengthInterval = this.+(-i)

  def intersect(that: LengthInterval): LengthInterval =
    val lower = this.lower max that.lower
    val upperOption =
      (this.upperOption, that.upperOption) match
        case (Some(iUpper), Some(oUpper)) => (iUpper min oUpper).some
        case (Some(iUpper), _) => iUpper.some
        case (_, Some(oUpper)) => oUpper.some
        case _ => none[Int]
    LengthInterval(lower, upperOption)

  def isEmpty: Boolean = upperOption.exists(upper => upper < 0 || upper < lower)
  def isPoint: Boolean = upperOption.exists(upper => upper >= 0 && upper == lower)
  def isAbove: Boolean = upperOption.isEmpty
  // def isBounded: Boolean = upperOption.exists(upper => upper >= 0 && upper >= lower)
  def toInterval: Interval[Int] =
    upperOption match
      case Some(upper) if upper < 0 || upper < lower => Interval.empty
      case Some(upper) if upper >= 0 && upper == lower => Interval.point(lower)
      case Some(upper) => Interval.closed(lower, upper)
      case None => Interval.atOrAbove(lower)

  override def toString: String =
    upperOption match
      case Some(upper) if upper < 0 || upper < lower => "(Ø)"
      case Some(upper) if upper >= 0 && upper == lower => s"[$lower]"
      case Some(upper) => s"[$lower, $upper]"
      case None => s"[$lower, ∞)"

end LengthInterval
object LengthInterval:
  case class UnboundedLengthInterval(lower: Int) extends LengthInterval:
    def upperOption: Option[Int] = None
  end UnboundedLengthInterval
  case class BoundedLengthInterval(lower: Int, upper: Int) extends LengthInterval:
    def upperOption: Option[Int] = Some(upper)
  end BoundedLengthInterval

  def apply(lower: Int): LengthInterval = UnboundedLengthInterval(lower)
  def apply(lower: Int, upper: Int): LengthInterval = BoundedLengthInterval(lower, upper)
  def apply(lower: Int, upperOption: Option[Int]): LengthInterval =
    upperOption.fold(UnboundedLengthInterval(lower))(upper => BoundedLengthInterval(lower, upper))
  val empty: LengthInterval = BoundedLengthInterval(0, 0)
  def atOrBelow(upper: Int): LengthInterval = BoundedLengthInterval(0, upper)
  def point(i: Int): LengthInterval = BoundedLengthInterval(i, i)

  def sum(head: LengthInterval, tail: List[LengthInterval]): LengthInterval =
    tail.foldLeft(head)((acc, current) => LengthInterval(
      acc.lower + current.lower,
      acc.upperOption.flatMap(accUpper => current.upperOption.map(accUpper + _))
    ))

  def sum(list: NonEmptyList[LengthInterval]): LengthInterval = sum(list.head, list.tail)

  def sum(list: List[LengthInterval]): LengthInterval =
    list match
      case head :: tail => sum(head, tail)
      case _ => empty

  def intersect(i: LengthInterval, o: LengthInterval): LengthInterval =
    val lower = i.lower max o.lower
    val upperOption =
      (i.upperOption, o.upperOption) match
        case (Some(iUpper), Some(oUpper)) => (iUpper min oUpper).some
        case (Some(iUpper), _) => iUpper.some
        case (_, Some(oUpper)) => oUpper.some
        case _ => none[Int]
    LengthInterval(lower, upperOption)
  end intersect
end LengthInterval
