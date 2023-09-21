package com.peknight.password.gen.charsets.option

case class Consecutive(
                        // 最大连续数量，连续字符数不能超过此数量
                        max: Int,
                        /*
                         * 最大步长限制，连续字符差值配置，如:
                         * 配置为0，则仅限制同一个字符不能相连，不支持"000"|"aaa"，支持"123"|"abc"
                         * 配置为1，则限制同一个字符或相邻字符不能相连，不支持"000"|"123"，支持"246"
                         * 配置为2，则限制同一个字符或相邻字符不能相连或步长为2字符不能相连，不支持"000"|"123"|"246"，支持"369"
                         */
                        step: Int,
                        // 是否大小写敏感
                        caseSensitive: Boolean = true
                      )
