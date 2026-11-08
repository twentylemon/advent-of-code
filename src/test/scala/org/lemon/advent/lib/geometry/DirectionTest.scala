package org.lemon.advent.lib.geometry

import org.lemon.advent.*
import org.lemon.advent.lib.geometry.*
import org.scalacheck.Prop.*
import org.scalacheck.*

class DirectionTest extends UnitTest:

  import Direction.*

  test("unitVector is a move from the origin") {
    check((dir: Direction) => Coord.origin.move(dir) == dir.unitVector)
  }

  Seq("Uu^" -> Up, "Ddv" -> Down, "Ll<" -> Left, "Rr>" -> Right).foreach((chars, dir) =>
    test(s"Direction parses each of $chars as $dir") {
      chars.foreach(c => Direction(c) shouldBe dir)
    }
  )

  test("Direction parses a String the same as a Char") {
    check((c: Char) => Direction.unapply(c.toString) == Direction.unapply(c))
  }

  test("Direction throws IllegalArgumentException for any other character") {
    check((c: Char) => !"UuDdLlRr^v<>".contains(c) ==> Prop.throws(classOf[IllegalArgumentException])(Direction(c)))
  }

  for
    (src, dest) <- Seq((Up, Left), (Left, Down), (Down, Right), (Right, Up))
  do
    test(s"$src turnLeft = $dest") {
      src.turnLeft shouldBe dest
    }

  for
    (src, dest) <- Seq((Up, Right), (Right, Down), (Down, Left), (Left, Up))
  do
    test(s"$src turnRight = $dest") {
      src.turnRight shouldBe dest
    }

  for
    (src, dest) <- Seq((Up, Down), (Down, Up), (Left, Right), (Right, Left))
  do
    test(s"$src turnAround = $dest") {
      src.turnAround shouldBe dest
    }
