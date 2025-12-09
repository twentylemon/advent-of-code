package org.lemon.advent.lib.`2d`

import scala.math.Integral.Implicits.*

private def `0`[N: Integral]: N = Integral[N].zero
private def `1`[N: Integral]: N = Integral[N].one
private def `-1`[N: Integral]: N = -Integral[N].one
private def fromInt[N: Integral](n: Int): N = Integral[N].fromInt(n)
