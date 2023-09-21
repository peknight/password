package com.peknight.password

import cats.Id
import com.peknight.password.interval.*
import com.peknight.password.length.LengthAllocate.*
import com.peknight.random.id.{LinearCongruentialRandom, Random as IdRandom}
import spire.math.Interval

object PasswordApp extends App:

  // ~ ！ _ @ . # * $ ^ &

  val r1 = IdRandom(System.currentTimeMillis())
  val r2 = LinearCongruentialRandom(1715456275261L)
  val r = r1

  println(allocate[Id, Int](Interval.above(119), Map(
    1 -> Interval.closed(50, 60),
    2 -> Interval.atOrBelow(50),
    3 -> Interval.closed(2, 10)
  )).map(_.runA(r)))

  println(generate(StringGenOption(
    Interval.point(16),
    Map(
      "num" -> CharsetOption((0 to 9).mkString, Interval.above(0), true),
      "lower" -> CharsetOption(('a' to 'z').mkString, Interval.above(0), true),
      "upper" -> CharsetOption(('A' to 'Z').mkString, Interval.above(0), true),
      "special" -> CharsetOption("~!_@.#*$^&", Interval.above(0), true)
    ),
    Some(Consecutive(1, 1)),
    7
  )).runA(r))

