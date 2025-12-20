package org.lemon.advent.year2015

private object Day13:

  def parse(input: String) = input.linesIterator.map(_ match
    case s"$from would $d $n happiness units by sitting next to $to." =>
      val amt = if d == "gain" then n.toInt else -n.toInt
      (from, to) -> amt
  ).toMap

  def seatPeeps(graph: Map[(String, String), Int], people: Seq[String]) =
    people.tail.permutations
      .map(arrangement =>
        val first = graph((arrangement.head, people.head)) + graph((people.head, arrangement.head))
        val last = graph((arrangement.last, people.head)) + graph((people.head, arrangement.last))
        val rest = arrangement.sliding(2).map { case Seq(from, to) => graph((from, to)) + graph((to, from)) }.sum
        first + last + rest
      )
      .max

  def part1(input: String) =
    val graph = parse(input)
    seatPeeps(graph, graph.keys.map(_._1).toSeq.distinct)

  def part2(input: String) =
    val graph = parse(input).withDefaultValue(0)
    seatPeeps(graph, graph.keys.map(_._1).toSeq.distinct :+ "Me")
