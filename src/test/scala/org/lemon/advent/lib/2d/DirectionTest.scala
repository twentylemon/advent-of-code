package org.lemon.advent.lib.`2d`

import org.lemon.advent.*
import org.lemon.advent.lib.`2d`.*
import org.scalacheck.Prop.*
import org.scalacheck.*

class DirectionTest extends UnitTest:

  import Direction.*

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
