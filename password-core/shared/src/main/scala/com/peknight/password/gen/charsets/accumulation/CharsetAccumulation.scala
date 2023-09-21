package com.peknight.password.gen.charsets.accumulation

import com.peknight.password.gen.charsets.interval.LengthInterval

case class CharsetAccumulation(chars: Vector[Char], length: LengthInterval, repeat: Boolean, startsWith: Boolean,
                               endsWith: Boolean)
