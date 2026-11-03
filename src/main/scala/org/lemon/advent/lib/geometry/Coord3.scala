package org.lemon.advent.lib.geometry

/** A 3d coordinate (or vector) with integer values. Integrates with [[Cuboid]].
  */
type Coord3 = Point3[Int]
object Coord3:
  val origin: Coord3 = Point3.origin
  val unitX: Coord3 = Point3.unitX
  val unitY: Coord3 = Point3.unitY
  val unitZ: Coord3 = Point3.unitZ

  def apply(x: Int, y: Int, z: Int): Coord3 = Point3(x, y, z)
