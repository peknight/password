package com.peknight.password

import cats.Id
import cats.syntax.option.*
import com.peknight.password.length.LengthAllocate.allocate
import com.peknight.random.id.Random as IdRandom
import spire.math.Interval

object PasswordApp extends App:

  // ~ ！ _ @ . # * $ ^ &

  println(allocate[Id, Int](Interval.above(119), Map(
    1 -> Interval.closed(50, 60),
    2 -> Interval.atOrBelow(50),
    3 -> Interval.closed(2, 10)
  )).map(_.runA(IdRandom(System.currentTimeMillis()))))

  case class Consecutive(maxExclusive: Int, step: Int)

  case class CharsetOption[+C <: Iterable[Char]](cs: C, length: Interval[Int], repeat: Boolean, consecutive: Option[Consecutive])

  case class PasswordOption[K, C <: Iterable[Char]](charsets: Map[K, CharsetOption[C]])
