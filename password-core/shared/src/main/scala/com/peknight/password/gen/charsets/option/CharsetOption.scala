package com.peknight.password.gen.charsets.option

import spire.math.Interval

case class CharsetOption[+C <: Iterable[Char]](
                                                // 字符集
                                                chars: C,
                                                // 此字符集长度区间
                                                length: Interval[Int] = Interval.atOrAbove(0),
                                                // 字符是否可重复
                                                repeat: Boolean = true,
                                                // 此字符集字符是否可用于生成的字符串开头
                                                startsWith: Boolean = true,
                                                // 此字符集字符是否可用于生成的字符串结尾
                                                endsWith: Boolean = true
                                              )
