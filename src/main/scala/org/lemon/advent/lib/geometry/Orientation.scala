package org.lemon.advent.lib.geometry

import org.lemon.advent.lib.*

import scala.math.Integral.Implicits.*

import Point3.*

object Orientation:
  private lazy val byAxes = values.map(orientation => ((orientation.facing, orientation.up), orientation)).toMap

  private def of(facing: Point3[Int], up: Point3[Int]): Orientation = byAxes((facing, up))

/** A facing and an up direction along the 3d axes. Applied to a point, it is the rotation that takes `unitRight` to
  * `facing` and `unitOut` to `up`.
  * @param facing the unit vector the orientation faces
  * @param up the unit vector pointing up, perpendicular to `facing`
  */
enum Orientation(val facing: Point3[Int], val up: Point3[Int]):
  case UpLeft extends Orientation(unitUp, unitLeft)
  case UpRight extends Orientation(unitUp, unitRight)
  case UpIn extends Orientation(unitUp, unitIn)
  case UpOut extends Orientation(unitUp, unitOut)
  case DownLeft extends Orientation(unitDown, unitLeft)
  case DownRight extends Orientation(unitDown, unitRight)
  case DownIn extends Orientation(unitDown, unitIn)
  case DownOut extends Orientation(unitDown, unitOut)
  case LeftUp extends Orientation(unitLeft, unitUp)
  case LeftDown extends Orientation(unitLeft, unitDown)
  case LeftIn extends Orientation(unitLeft, unitIn)
  case LeftOut extends Orientation(unitLeft, unitOut)
  case RightUp extends Orientation(unitRight, unitUp)
  case RightDown extends Orientation(unitRight, unitDown)
  case RightIn extends Orientation(unitRight, unitIn)
  case RightOut extends Orientation(unitRight, unitOut)
  case InUp extends Orientation(unitIn, unitUp)
  case InDown extends Orientation(unitIn, unitDown)
  case InLeft extends Orientation(unitIn, unitLeft)
  case InRight extends Orientation(unitIn, unitRight)
  case OutUp extends Orientation(unitOut, unitUp)
  case OutDown extends Orientation(unitOut, unitDown)
  case OutLeft extends Orientation(unitOut, unitLeft)
  case OutRight extends Orientation(unitOut, unitRight)

  def turnLeft: Orientation = Orientation.of(up.cross(facing), up)
  def turnRight: Orientation = Orientation.of(facing.cross(up), up)
  def turnUp: Orientation = Orientation.of(up, -facing)
  def turnDown: Orientation = Orientation.of(-up, facing)
  def turnAround: Orientation = Orientation.of(-facing, up)

  def apply[N: Integral](point: Point3[N]): Point3[N] =
    def along(axis: Point3[Int], n: N) = Point3(fromInt[N](axis.x) * n, fromInt[N](axis.y) * n, fromInt[N](axis.z) * n)
    along(facing, point.x) + along(facing.cross(up), point.y) + along(-up, point.z)
