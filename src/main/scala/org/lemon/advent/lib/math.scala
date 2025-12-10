package org.lemon.advent.lib

import scala.math.Integral.Implicits.*
import scala.math.Ordering.Implicits.*

def `0`[N: Integral]: N = Integral[N].zero
def `1`[N: Integral]: N = Integral[N].one
def `-1`[N: Integral]: N = -Integral[N].one
def fromInt[N: Integral](n: Int): N = Integral[N].fromInt(n)

extension [N: Integral](a: N)
  /** Returns the positive modulo of this mod `n`. If the regular modulo is negative, this
    * will shift it back into the positive ranges.
    *
    * @param n the modulus
    * @return the positive modulo of this mod `n`
    */
  def +%(n: N): N =
    val mod = a % n
    if mod < Integral[N].zero then mod + n else mod

  /** Returns the greatest common divisor of this and `b`.
    *
    * @param b the other number
    * @return the greatest common divisor of this and `b`
    */
  def gcd(b: N): N =
    @annotation.tailrec
    def loop(a: N, b: N): N = if b == Integral[N].zero then a else loop(b, a % b)
    loop(a, b)

  /** Returns the least common multiple of this and `b`.
    *
    * @param b the other number
    * @return the least common multiple of this and `b`
    */
  def lcm(b: N): N = a * b / a.gcd(b)

extension [N: Integral](it: Iterable[N])
  /** @return the greatest common divisor of all elements in the iterable */
  def gcd: N = it.reduce(_ `gcd` _)

  /** @return the least common multiple of all elements in the iterable */
  def lcm: N = it.reduce(_ `lcm` _)
