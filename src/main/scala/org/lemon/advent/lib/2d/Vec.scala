package org.lemon.advent.lib.`2d`

import org.lemon.advent.lib.*

import scala.math.Integral.Implicits.*

/** A 2d vector with integral values. Contains some extensions over [[Point]]
  * which only make sense in a vector context.
  */
opaque type VecT[N] = Point[N]
type Vec = VecT[Int]
object VecT:
  def apply[N: Integral](coord: Point[N]): VecT[N] = coord
  def apply[N: Integral](x: N, y: N): VecT[N] = Point[N](x, y)

  given [N: Integral]: Conversion[VecT[N], Point[N]] = identity

  extension [N: Integral](vec: VecT[N])
    def reduce: VecT[N] =
      if vec.x == `0` then (`0`, vec.y.sign)
      else if vec.y == `0` then (vec.x.sign, `0`)
      else
        val gcd = vec.x.abs.gcd(vec.y.abs)
        (vec.x / gcd, vec.y / gcd)
