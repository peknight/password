package com.peknight.password.gen.charsets.option

import spire.math.Interval

case class GenOption[K, C <: Iterable[Char]](
                                              // 生成使用的字符集
                                              charsets: Map[K, CharsetOption[C]],
                                              // 生成长度区间
                                              length: Interval[Int],
                                              // 连续性限制
                                              consecutive: Option[Consecutive] = None,
                                              // 生成失败重试次数
                                              retry: Int = 3
                                            )
