package com.peknight.password.gen.charsets.accumulation

case class GenAccumulation[K](remain: Int, chars: List[Char], charsetAccumulations: Map[K, CharsetAccumulation],
                              consecutiveAccumulation: Option[ConsecutiveAccumulation])
