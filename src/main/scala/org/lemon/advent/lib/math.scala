package org.lemon.advent.lib

import scala.math.Integral.Implicits.*
import scala.math.Ordering.Implicits.*

def `0`[N: Integral]: N = Integral[N].zero
def `1`[N: Integral]: N = Integral[N].one
def `-1`[N: Integral]: N = -Integral[N].one
def fromInt[N: Integral](n: Int): N = Integral[N].fromInt(n)
def fromString[N: Integral](s: String): N = Integral[N].parseString(s).get

extension [N: Integral](a: N)
  /** Returns the positive modulo of this mod `n`. If the regular modulo is negative, this
    * will shift it back into the positive ranges.
    *
    * @param n the modulus
    * @return the positive modulo of this mod `n`
    */
  def +%(n: N): N =
    val mod = a % n
    if mod < `0` then mod + n else mod

  /** Returns `this / n` rounded up.
    *
    * @param n the divisor
    * @return the result of division, rounding up if necessary
    */
  def /^(n: N): N =
    val (quotient, remainder) = a /% n
    // division truncates towards zero, which already rounded up if the true quotient is negative
    if remainder != `0` && (remainder > `0`) == (n > `0`) then quotient + `1` else quotient

  /** Returns the greatest common divisor of this and `b`, which is never negative unless it does not fit in `N`:
    * the gcd of `MinValue` with `0` or `MinValue` is `-MinValue`, which overflows to `MinValue` like `abs` does.
    * That is still a divisor of the right size, so it still works as an argument to another `gcd`.
    *
    * @param b the other number
    * @return the greatest common divisor of this and `b`
    */
  def gcd(b: N): N =
    @annotation.tailrec
    def loop(a: N, b: N): N = if b == `0` then a.abs else loop(b, a % b)
    loop(a, b)

  /** Returns the least common multiple of this and `b`.
    *
    * @param b the other number
    * @return the least common multiple of this and `b`
    */
  def lcm(b: N): N = if a == `0` || b == `0` then `0` else (a / a.gcd(b) * b).abs

extension [N: Integral](it: Iterable[N])
  /** @return the greatest common divisor of all elements in the iterable */
  def gcd: N = it.reduce(_ `gcd` _)

  /** @return the least common multiple of all elements in the iterable */
  def lcm: N = it.reduce(_ `lcm` _)
