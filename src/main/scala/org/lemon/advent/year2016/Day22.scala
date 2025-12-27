package org.lemon.advent.year2016

import org.lemon.advent.lib.*
import org.lemon.advent.lib.`2d`.*
import org.lemon.advent.lib.graph.*

private object Day22:

  case class Disk(loc: Coord, size: Int, used: Int, avail: Int)

  def parse(input: String) =
    def amt(str: String) =
      str match
        case s"${size}T" => size.toInt
        case s"${size}%" => size.toInt

    input.linesIterator.drop(2).map(line =>
      val Seq(loc, size, used, avail, _) = line.wsv
      loc match
        case s"/dev/grid/node-x$x-y$y" => Disk((x.toInt, y.toInt), amt(size), amt(used), amt(avail))
    ).toIndexedSeq

  def part1(input: String) =
    val disks = parse(input)
    val orderedPairs =
      for
        lhs <- disks.iterator
        rhs <- disks.iterator
        if lhs != rhs
      yield (lhs, rhs)
    orderedPairs.count((lhs, rhs) => lhs.used > 0 && lhs.used <= rhs.avail)

  case class Node(empty: Coord, goal: Coord)

  def adjacency(movable: Set[Coord])(node: Node) = node.empty.adjacent
    .filter(movable)
    .map(n =>
      if n == node.goal then node.copy(empty = n, goal = node.empty)
      else node.copy(empty = n)
    )

  def part2(input: String) =
    val disks = parse(input)
    val movable = disks
      .filter(disk => disk.loc.adjacent.forall(n => disks.find(_.loc == n).forall(_.size >= disk.used)))
      .map(_.loc)
      .toSet
    val start = Node(empty = disks.find(_.used == 0).get.loc, goal = disks.filter(_.loc.y == 0).maxBy(_.loc.x).loc)
    pathFind(adjacency(movable), _.goal.manhattan((0, 0)), start, _.goal == Coord(0, 0)).get.distance
