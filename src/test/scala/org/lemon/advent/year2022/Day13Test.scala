package org.lemon.advent.year2022

import io.circe._
import io.circe.syntax._
import io.circe.parser._
import scala.math.Ordering.Implicits._
import org.lemon.advent._

class Day13Test extends UnitTest {

  def parseJson(line: String) = parse(line) match
    case Right(thing) => thing
    case Left(_) => throw AssertionError()

  extension (json: Json)
    def asInt = json.asNumber.get.toInt.get
    def asSeq: Seq[Json] = json.asArray.get

  given Ordering[Json] with
    def compare(lhs: Json, rhs: Json): Int =
      (lhs, rhs) match
        case (l, r) if l.isNumber && r.isNumber => l.asInt - r.asInt
        case (l, r) if l.isNumber && r.isArray => compare(Seq(l.asInt).asJson, r)
        case (l, r) if l.isArray && r.isNumber => compare(l, Seq(r.asInt).asJson)
        case (l, r) if l.isArray && r.isArray =>
          (l.asSeq, r.asSeq) match
            case (Seq(x, xs*), Seq(y, ys*)) =>
              val comp = compare(x, y)
              if comp != 0 then comp else compare(xs.asJson, ys.asJson)
            case (Seq(xs*), Seq(ys*)) => xs.size - ys.size
        case _ => throw AssertionError()

  def part1(in: String) = in.split("\n\n")
    .map(_.linesIterator.toSeq)
    .map(lines => (parseJson(lines(0)), parseJson(lines(1))))
    .zipWithIndex
    .filter({ case ((lhs, rhs), _) => lhs <= rhs })
    .map(_._2 + 1)
    .sum

  def part2(in: String) =
    val key1 = "[[2]]"
    val key2 = "[[6]]"
    val packets = in.split("\n\n").flatMap(_.linesIterator).appendedAll(Seq(key1, key2)).map(parseJson).sorted
    (packets.indexOf(parseJson(key1)) + 1) * (packets.indexOf(parseJson(key2)) + 1)

  test("part 1 example") {
    val in = """|[1,1,3,1,1]
                |[1,1,5,1,1]
                |
                |[[1],[2,3,4]]
                |[[1],4]
                |
                |[9]
                |[[8,7,6]]
                |
                |[[4,4],4,4]
                |[[4,4],4,4,4]
                |
                |[7,7,7,7]
                |[7,7,7]
                |
                |[]
                |[3]
                |
                |[[[]]]
                |[[]]
                |
                |[1,[2,[3,[4,[5,6,7]]]],8,9]
                |[1,[2,[3,[4,[5,6,0]]]],8,9]""".stripMargin

    part1(in) shouldBe 13
  }

  test("part 1") {
    part1(read(file(2022)(13))) shouldBe 6272
  }

  test("part 2 example") {
    val in = """|[1,1,3,1,1]
                |[1,1,5,1,1]
                |
                |[[1],[2,3,4]]
                |[[1],4]
                |
                |[9]
                |[[8,7,6]]
                |
                |[[4,4],4,4]
                |[[4,4],4,4,4]
                |
                |[7,7,7,7]
                |[7,7,7]
                |
                |[]
                |[3]
                |
                |[[[]]]
                |[[]]
                |
                |[1,[2,[3,[4,[5,6,7]]]],8,9]
                |[1,[2,[3,[4,[5,6,0]]]],8,9]""".stripMargin

    part2(in) shouldBe 140
  }

  test("part 2") {
    part2(read(file(2022)(13))) shouldBe 22288
  }

}
