package org.lemon.advent.year2016

import org.lemon.advent.lib.*
import org.lemon.advent.lib.`2d`.*
import org.lemon.advent.lib.graph.*

private object Day24:

  def parse = Coord.gridToMap

  def vents(input: String) =
    val grid = parse(input)
    val adjacency = grid
      .filter((_, at) => at != '#')
      .map((coord, _) => (coord, coord.adjacent.filter(c => grid.getOrElse(c, '#') != '#')))
    val checkpoints = grid.filter((_, at) => at.isDigit)
    val invert = grid.map(_.swap)
    val nodes = junctions(adjacency) ++ checkpoints.keys
    val graph = compress(adjacency, nodes)
    ('0' to checkpoints.values.max).pairs
      .map((from, to) => (from, to) -> pathFind(graph, invert(from), invert(to)).get.distance)
      .flatMap((path, dist) => Seq(path -> dist, path.swap -> dist))
      .toMap

  def shortestRoute(route: Seq[Char], distances: Map[(Char, Char), Int]) =
    route.sliding2.map(distances).sum

  def part1(input: String) =
    val distances = vents(input)
    ('1' to distances.keys.map(_._2).max).permutations
      .map('0' +: _)
      .map(shortestRoute(_, distances))
      .min

  def part2(input: String) =
    val distances = vents(input)
    ('1' to distances.keys.map(_._2).max).permutations
      .map('0' +: _ :+ '0')
      .map(shortestRoute(_, distances))
      .min
