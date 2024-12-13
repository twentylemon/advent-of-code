package org.lemon.advent.lib

import scala.math.Integral.Implicits._
import scala.math.Ordering.Implicits._

extension [N: Integral](a: N)
  def +%(n: N): N =
    val mod = a % n
    if mod < Integral[N].zero then mod + n else mod

  def gcd(b: N): N =
    @annotation.tailrec
    def loop(a: N, b: N): N = if b == Integral[N].zero then a else loop(b, a % b)
    loop(a, b)

  def lcm(b: N): N = a * b / a.gcd(b)
