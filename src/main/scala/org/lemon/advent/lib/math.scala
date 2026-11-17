package org.lemon.advent.lib

import scala.math.Integral.Implicits.*
import scala.math.Ordering.Implicits.*

def `0`[N: Integral]: N = Integral[N].zero
def `1`[N: Integral]: N = Integral[N].one
def `-1`[N: Integral]: N = -Integral[N].one
def fromInt[N: Integral](n: Int): N = Integral[N].fromInt(n)
def fromString[N: Integral](s: String): N = Integral[N].parseString(s).get

extension [N: Integral](a: N)
  /** @return this modulo `n`, in `[0, |n|)` */
  def +%(n: N): N =
    val mod = a % n
    if mod < `0` then mod + n.abs else mod

  /** @return `this / n`, rounded up */
  def /^(n: N): N =
    val (quotient, remainder) = a /% n
    if remainder != `0` && (remainder > `0`) == (n > `0`) then quotient + `1` else quotient

  /** @return the greatest common divisor of this and `b` */
  def gcd(b: N): N =
    @annotation.tailrec
    def loop(a: N, b: N): N = if b == `0` then a.abs else loop(b, a % b)
    loop(a, b)

  /** @return the least common multiple of this and `b` */
  def lcm(b: N): N = if a == `0` || b == `0` then `0` else (a / a.gcd(b) * b).abs

extension [N: Integral](it: Iterable[N])
  /** @return the greatest common divisor of all elements in the iterable
    * @throws UnsupportedOperationException if the iterable is empty
    */
  def gcd: N = it.reduce(_ `gcd` _)

  /** @return the least common multiple of all elements in the iterable
    * @throws UnsupportedOperationException if the iterable is empty
    */
  def lcm: N = it.reduce(_ `lcm` _)
