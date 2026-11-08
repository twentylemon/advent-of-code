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
    chars.foreach(c =>
      test(s"Direction parses '$c' as $dir") {
        Direction(c) shouldBe dir
      }

      test(s"Direction parses \"$c\" as $dir") {
        Direction(c.toString) shouldBe dir
      }
    )
  )

  test("Direction throws IllegalArgumentException for any other Char") {
    check((c: Char) => !"UuDdLlRr^v<>".contains(c) ==> Prop.throws(classOf[IllegalArgumentException])(Direction(c)))
  }

  test("Direction throws IllegalArgumentException for any other String") {
    check((str: String) =>
      !(str.length == 1 && "UuDdLlRr^v<>".contains(str)) ==>
        Prop.throws(classOf[IllegalArgumentException])(Direction(str))
    )
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
