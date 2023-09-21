package com.peknight.password.interval

sealed trait LengthInterval:
  def lower: Int
  def upperOption: Option[Int]

  def +(i: Int): LengthInterval = LengthInterval(
    if lower + i > 0 then lower + i else 0,
    upperOption.map(upper => if upper + i > 0 then upper + i else 0)
  )

  def -(i: Int): LengthInterval = this.+(-i)

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
end LengthInterval
