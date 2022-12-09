package org.lemon.advent.year2022

import scala.collection.mutable
import org.lemon.UnitTest
import scala.util.Using
import scala.io.Source

class Day09Test extends UnitTest {

  type Coord = (Int, Int)
  extension (coord: Coord)
    def x = coord._1
    def y = coord._2

  type BigRope = Seq[Coord]
  extension (rope: BigRope)
    def headKnot = rope.head
    def tailKnot = rope.last

  def drag(head: Coord, tail: Coord): Coord = (head, tail) match
    case ((hx, hy), (tx, ty)) if (hx-tx).abs == 2 && hy == ty => ((hx+tx)/2, ty)
    case ((hx, hy), (tx, ty)) if (hy-ty).abs == 2 && hx == tx => (tx, (hy+ty)/2)
    case ((hx, hy), (tx, ty)) if (hx-tx).abs == 1 && (hy-ty).abs == 2 => (hx, (hy+ty)/2)
    case ((hx, hy), (tx, ty)) if (hx-tx).abs == 2 && (hy-ty).abs == 1 => ((hx+tx)/2, hy)
    case ((hx, hy), (tx, ty)) if (hx-tx).abs == 2 && (hy-ty).abs == 2 => ((hx+tx)/2, (hy+ty)/2)
    case _ => tail

  def update(rope: BigRope, step: (Coord => Coord), n: Int, tailPos: mutable.Map[Coord, Int]): BigRope =
    var head = rope.headKnot
    var tail = rope.tailKnot
    var newRope = mutable.Seq(rope: _*)
    for _ <- 1 to n
    do
      newRope(0) = step(newRope(0))
      for i <- 1 to rope.size - 1 do newRope(i) = drag(newRope(i-1), newRope(i))
      tailPos(newRope.last) += 1
    newRope.toSeq

  def move(move: String, rope: BigRope, tailPos: mutable.Map[Coord, Int]) = move match
    case s"L $n" => update(rope, h => (h.x - 1, h.y), n.toInt, tailPos)
    case s"R $n" => update(rope, h => (h.x + 1, h.y), n.toInt, tailPos)
    case s"U $n" => update(rope, h => (h.x, h.y + 1), n.toInt, tailPos)
    case s"D $n" => update(rope, h => (h.x, h.y - 1), n.toInt, tailPos)

  def part1(in: String) =
    val tailPos = mutable.Map.empty[Coord, Int].withDefaultValue(0)
    var rope = Seq((0, 0), (0, 0))
    in.linesIterator.foreach(m => rope = move(m, rope, tailPos))
    tailPos.size

  def part2(in: String) =
    val tailPos = mutable.Map.empty[Coord, Int].withDefaultValue(0)
    var rope = Seq.fill(10)((0, 0))
    in.linesIterator.foreach(m => rope = move(m, rope, tailPos))
    tailPos.size

  test("part 1 example") {
    val in = """|R 4
                |U 4
                |L 3
                |D 1
                |R 4
                |D 1
                |L 5
                |R 2""".stripMargin

    part1(in) shouldBe 13
  }

  test("part 1") {
    Using.resource(Source.fromResource("year2022/day09.txt"))(source =>
      part1(source.mkString) shouldBe 6503
    )
  }

  test("part 2 example 1") {
    val in = """|R 4
                |U 4
                |L 3
                |D 1
                |R 4
                |D 1
                |L 5
                |R 2""".stripMargin

    part2(in) shouldBe 1
  }

  test("part 2 example 2") {
    val in = """|R 5
                |U 8
                |L 8
                |D 3
                |R 17
                |D 10
                |L 25
                |U 20""".stripMargin

    part2(in) shouldBe 36
  }

  test("part 2" ) {
    Using.resource(Source.fromResource("year2022/day09.txt"))(source =>
      part2(source.mkString) shouldBe 2724
    )
  }
}
