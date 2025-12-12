package org.lemon.advent.year2022

private object Day11:

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
      .groupBy(monkey.target)
      .map((i, queue) => monkeys(i).copy(items = monkeys(i).items ++ queue))
      .map(m => m.id -> m)
      .toMap
    monkeys ++ toQueue ++ Map(id -> monkey.copy(
      items = Seq(),
      totalInspected = monkey.totalInspected + monkey.items.size
    ))

  def round(relax: BigInt => BigInt)(monkeys: Map[Int, Monkey]): Map[Int, Monkey] =
    monkeys.keysIterator.toSeq.sorted.foldLeft(monkeys)((m, id) => turn(relax)(id)(m))

  def part1(input: String) =
    val monkeys = input.split("\n\n").map(parseMonkey).map(m => m.id -> m).toMap
    val ending = (1 to 20).foldLeft(monkeys)((m, _) => round(x => x / 3)(m))
    ending.values.map(_.totalInspected).toSeq.sorted.takeRight(2).reduce(_ * _)

  def part2(input: String) =
    val monkeys = input.split("\n\n").map(parseMonkey).map(m => m.id -> m).toMap
    val mod = monkeys.values.map(_.divisor).reduce(_ * _)
    val ending = (1 to 10000).foldLeft(monkeys)((m, i) => round(x => x % mod)(m))
    ending.values.map(_.totalInspected).toSeq.sorted.takeRight(2).map(_.toLong).reduce(_ * _)
