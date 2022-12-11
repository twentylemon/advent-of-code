package org.lemon.advent.year2022

import org.lemon.advent._

class Day11Test extends UnitTest {

  case class Monkey(
      id: Int,
      items: Seq[BigInt],
      inspect: BigInt => BigInt,
      target: BigInt => Int,
      divisor: Int,
      totalInspected: Int = 0
  )

  def parseInspect(op: String): BigInt => BigInt =
    op.strip match
      case "Operation: new = old + old" => (old: BigInt) => old + old
      case "Operation: new = old * old" => (old: BigInt) => old * old
      case s"Operation: new = old + $rhs" => (old: BigInt) => old + rhs.toInt
      case s"Operation: new = old * $rhs" => (old: BigInt) => old * rhs.toInt

  def parseTarget(test: String, ifTrue: String, ifFalse: String): BigInt => Int =
    val trueTarget = ifTrue.strip.split(" ").last.toInt
    val falseTarget = ifFalse.strip.split(" ").last.toInt
    test.strip match
      case s"Test: divisible by $n" => (x: BigInt) => if x % n.toInt == 0 then trueTarget else falseTarget

  def parseDivisor(test: String): Int =
    test.strip match
      case s"Test: divisible by $n" => n.toInt

  def parseMonkey(in: String) =
    val lines = in.linesIterator.map(_.strip).toSeq
    val id = lines(0) match { case s"Monkey $i:" => i.toInt }
    val items = lines(1).strip match { case s"Starting items: $list" => list.split(", ").map(BigInt(_)) }
    val inspect = parseInspect(lines(2))
    val target = parseTarget(lines(3), lines(4), lines(5))
    val divisor = parseDivisor(lines(3))
    Monkey(id = id, items = items, inspect = inspect, target = target, divisor = divisor)

  def turn(relax: BigInt => BigInt)(id: Int)(monkeys: Map[Int, Monkey]): Map[Int, Monkey] =
    val monkey = monkeys(id)
    val toQueue = monkey.items
      .map(worry => monkey.inspect(worry))
      .map(relax)
      .map(worry => (worry, monkey.target(worry)))
      .groupBy(_._2)
      .mapValues(_.map(_._1))
      .map((i, queue) => monkeys(i).copy(items = monkeys(i).items ++ queue))
      .map(m => m.id -> m)
      .toMap
    monkeys ++ toQueue ++ Map(id -> monkey.copy(
      items = Seq(),
      totalInspected = monkey.totalInspected + monkey.items.size
    ))

  def round(relax: BigInt => BigInt)(monkeys: Map[Int, Monkey]): Map[Int, Monkey] =
    monkeys.keysIterator.toSeq.sorted.foldLeft(monkeys)((m, id) => turn(relax)(id)(m))

  def part1(in: String) =
    val monkeys = in.split("\n\n").map(parseMonkey).map(m => m.id -> m).toMap
    val ending = (1 to 20).foldLeft(monkeys)((m, _) => round(x => x / 3)(m))
    ending.values.map(_.totalInspected).toSeq.sorted.takeRight(2).reduce(_ * _)

  def part2(in: String) =
    val monkeys = in.split("\n\n").map(parseMonkey).map(m => m.id -> m).toMap
    val mod = monkeys.values.map(_.divisor).reduce(_ * _)
    val ending = (1 to 10000).foldLeft(monkeys)((m, i) => round(x => x % mod)(m))
    ending.values.map(_.totalInspected).toSeq.sorted.takeRight(2).map(_.toLong).reduce(_ * _)

  test("part 1 example") {
    val in = """|Monkey 0:
                |  Starting items: 79, 98
                |  Operation: new = old * 19
                |  Test: divisible by 23
                |    If true: throw to monkey 2
                |    If false: throw to monkey 3
                |
                |Monkey 1:
                |  Starting items: 54, 65, 75, 74
                |  Operation: new = old + 6
                |  Test: divisible by 19
                |    If true: throw to monkey 2
                |    If false: throw to monkey 0
                |
                |Monkey 2:
                |  Starting items: 79, 60, 97
                |  Operation: new = old * old
                |  Test: divisible by 13
                |    If true: throw to monkey 1
                |    If false: throw to monkey 3
                |
                |Monkey 3:
                |  Starting items: 74
                |  Operation: new = old + 3
                |  Test: divisible by 17
                |    If true: throw to monkey 0
                |    If false: throw to monkey 1""".stripMargin

    part1(in) shouldBe 10605
  }

  test("part 1") {
    part1(read(file(2022)(11))) shouldBe 100345
  }

  test("part 2 example") {
    val in = """|Monkey 0:
                |  Starting items: 79, 98
                |  Operation: new = old * 19
                |  Test: divisible by 23
                |    If true: throw to monkey 2
                |    If false: throw to monkey 3
                |
                |Monkey 1:
                |  Starting items: 54, 65, 75, 74
                |  Operation: new = old + 6
                |  Test: divisible by 19
                |    If true: throw to monkey 2
                |    If false: throw to monkey 0
                |
                |Monkey 2:
                |  Starting items: 79, 60, 97
                |  Operation: new = old * old
                |  Test: divisible by 13
                |    If true: throw to monkey 1
                |    If false: throw to monkey 3
                |
                |Monkey 3:
                |  Starting items: 74
                |  Operation: new = old + 3
                |  Test: divisible by 17
                |    If true: throw to monkey 0
                |    If false: throw to monkey 1""".stripMargin

    part2(in) shouldBe 2713310158L
  }

  test("part 2") {
    part2(read(file(2022)(11))) shouldBe 28537348205L
  }
}
