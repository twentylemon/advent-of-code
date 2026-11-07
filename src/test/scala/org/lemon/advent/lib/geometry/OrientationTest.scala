package org.lemon.advent.lib.geometry

import org.lemon.advent.*
import org.lemon.advent.lib.geometry.*
import org.scalacheck.Prop.*
import org.scalacheck.*

class OrientationTest extends UnitTest:

  import Orientation.*

  def onGrid(direction: Direction): Orientation =
    val facing = Point3(direction.unitVector.x, direction.unitVector.y, 0)
    values.find(o => o.facing == facing && o.up == Point3.unitOut[Int]).get

  test("orientations are the perpendicular pairs of unit vectors") {
    val units = Point3.origin[Int].adjacent
    val pairs = for facing <- units; up <- units if facing.dot(up) == 0 yield (facing, up)
    values.map(o => (o.facing, o.up)).toSeq should contain theSameElementsAs pairs
  }

  test("turnLeft and turnRight are opposites") {
    check((o: Orientation) => o.turnLeft.turnRight == o)
  }

  test("turnUp and turnDown are opposites") {
    check((o: Orientation) => o.turnUp.turnDown == o)
  }

  test("turnAround twice is the same orientation") {
    check((o: Orientation) => o.turnAround.turnAround == o)
  }

  test("turnLeft twice is turnAround") {
    check((o: Orientation) => o.turnLeft.turnLeft == o.turnAround)
  }

  test("turnRight twice is turnAround") {
    check((o: Orientation) => o.turnRight.turnRight == o.turnAround)
  }

  test("turnUp four times is the same orientation") {
    check((o: Orientation) => o.turnUp.turnUp.turnUp.turnUp == o)
  }

  test("turnLeft and turnRight keep up") {
    check((o: Orientation) => o.turnLeft.up == o.up && o.turnRight.up == o.up)
  }

  test("turnUp faces up") {
    check((o: Orientation) => o.turnUp.facing == o.up)
  }

  test("turnDown faces down") {
    check((o: Orientation) => o.turnDown.facing == -o.up)
  }

  test("turnAround faces the opposite way and keeps up") {
    check((o: Orientation) => o.turnAround.facing == -o.facing && o.turnAround.up == o.up)
  }

  test("turnLeft with up out is the 2d turnLeft") {
    check((d: Direction) => onGrid(d).turnLeft == onGrid(d.turnLeft))
  }

  test("turnRight with up out is the 2d turnRight") {
    check((d: Direction) => onGrid(d).turnRight == onGrid(d.turnRight))
  }

  test("turnAround with up out is the 2d turnAround") {
    check((d: Direction) => onGrid(d).turnAround == onGrid(d.turnAround))
  }
