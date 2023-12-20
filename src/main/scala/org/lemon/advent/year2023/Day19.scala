package org.lemon.advent.year2023

private object Day19:

  type Gear = Map[Char, Int]
  case class Flow(v: Char, op: Char, lit: Int, dest: String)

  def parseGears(input: String) = input.linesIterator
    .map(_ match
      case s"{x=$x,m=$m,a=$a,s=$s}" => Map('x' -> x.toInt, 'm' -> m.toInt, 'a' -> a.toInt, 's' -> s.toInt)
    )
    .toSeq

  def parseFlow(flow: String) = flow.split(",")
    .map(_ match
      case s"$v>$n:$dest" => Flow(v = v.head, op = '>', lit = n.toInt, dest = dest)
      case s"$v<$n:$dest" => Flow(v = v.head, op = '<', lit = n.toInt, dest = dest)
      case otherwise => Flow(v = 'x', op = '>', lit = -1, dest = otherwise)
    )
    .toSeq

  def parseFlows(input: String) = input.linesIterator
    .map(_ match
      case s"$name{$flow}" => (name, parseFlow(flow))
    )
    .toMap

  def parse(input: String) =
    val Array(flows, gears) = input.split("\n\n")
    (parseFlows(flows), parseGears(gears))

  def compare(gear: Gear, v: Char, op: Char, lit: Int) =
    op match
      case '>' => gear(v) > lit
      case '<' => gear(v) < lit

  def accepted(gear: Gear, workflows: Map[String, Seq[Flow]], at: String): Boolean =
    if at == "R" then false
    else if at == "A" then true
    else
      workflows(at).find { case Flow(v, op, lit, _) => compare(gear, v, op, lit) }
        .map(f => accepted(gear, workflows, f.dest))
        .get

  def part1(input: String) =
    val (flows, gears) = parse(input)
    gears
      .filter(accepted(_, flows, "in"))
      .flatMap(_.values)
      .sum

  def countAccepted(flow: Seq[Flow], ranges: Map[Char, Range], workflows: Map[String, Seq[Flow]]): Long =
    val Flow(v, op, lit, dest) = flow.head
    val (good, bad) = op match
      case '>' =>
        val (g, b) = ranges(v).reverse.span(_ > lit)
        (g.reverse, b.reverse)
      case '<' => ranges(v).span(_ < lit)

    val badCount =
      if flow.tail.isEmpty || bad.isEmpty then 0L else countAccepted(flow.tail, ranges.updated(v, bad), workflows)

    val goodCount =
      if good.isEmpty then 0
      else
        dest match
          case "A" => ranges.updated(v, good).values.map(_.size.toLong).product
          case "R" => 0
          case dest => countAccepted(workflows(dest), ranges.updated(v, good), workflows)

    goodCount + badCount

  def part2(input: String) =
    val (flows, _) = parse(input)
    countAccepted(flows("in"), "xmas".map(_ -> (1 to 4000)).toMap, flows)
