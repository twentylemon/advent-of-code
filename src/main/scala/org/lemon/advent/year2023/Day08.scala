package org.lemon.advent.year2023

import org.lemon.advent.lcm

private object Day08:

  def parse(input: String) =
    val Array(directions, tail) = input.split("\n\n")
    val graph = tail.linesIterator
      .map(_ match
        case s"$from = ($l, $r)" => (from, (l, r))
      )
      .toMap
    (directions, graph)

  def move(loc: String, direction: Char, graph: Map[String, (String, String)]) =
    if direction == 'L' then graph(loc)._1 else graph(loc)._2

  def walk(directions: String, graph: Map[String, (String, String)], start: String) =
    Iterator.iterate((start, 0))((loc, step) => (move(loc, directions(step % directions.length), graph), step + 1))

  def part1(input: String) =
    val (directions, graph) = parse(input)
    walk(directions, graph, "AAA")
      .dropWhile(_._1 != "ZZZ")
      .next._2

  def part2(input: String) =
    val (directions, graph) = parse(input)

    val cycle = graph.keySet.toSeq
      .filter(_.endsWith("A"))
      .map(walk(directions, graph, _))
      .map(_.dropWhile(!_._1.endsWith("Z")))
      .map(_.next._2.toLong)

    cycle.fold(1L)(lcm)
