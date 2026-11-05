package org.lemon.advent.lib.geometry

import Point3.*

object Orientation:
  private val all = IArray.from(values)
  private def of(facing: Point3[Int], up: Point3[Int]) = all.find(o => o.facing == facing && o.up == up).get

  private val lefts = all.map(o => of(o.up.cross(o.facing), o.up))
  private val rights = all.map(o => of(o.facing.cross(o.up), o.up))
  private val ups = all.map(o => of(o.up, -o.facing))
  private val downs = all.map(o => of(-o.up, o.facing))
  private val arounds = all.map(o => of(-o.facing, o.up))

/** A facing and an up direction along the 3d axes.
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

  def turnLeft: Orientation = Orientation.lefts(ordinal)
  def turnRight: Orientation = Orientation.rights(ordinal)
  def turnUp: Orientation = Orientation.ups(ordinal)
  def turnDown: Orientation = Orientation.downs(ordinal)
  def turnAround: Orientation = Orientation.arounds(ordinal)
