package org.lemon.advent.year2023

import org.lemon.advent.lib.`2d`._

import scala.collection.mutable

private object Day23:

  def parse = Coord.gridToMap

  def startOf(grid: Map[Coord, Char]) = Area(grid).topRow.find(grid(_) == '.').get
  def endOf(grid: Map[Coord, Char]) = Area(grid).bottomRow.find(grid(_) == '.').get

  def neighbours(from: Coord, grid: Map[Coord, Char]) =
    grid(from) match
      case '<' => Seq(from.left)
      case '>' => Seq(from.right)
      case '^' => Seq(from.up)
      case 'v' => Seq(from.down)
      case _ => from.adjacent

  def scenicRoute(grid: Map[Coord, Char], start: Coord, end: Coord, edges: Map[Coord, Seq[(Coord, Int)]]) =
    val paths = mutable.Buffer.empty[Int]
    val queue = mutable.Queue((Set(start), start, 0))
    while !queue.isEmpty do
      val (path, cell, steps) = queue.dequeue
      if cell == end then paths += steps
      else
        queue ++= edges(cell)
          .filterNot((coord, _) => path.contains(coord))
          .map((coord, dist) => (path + coord, coord, steps + dist))

    paths.max

  def part1(input: String) =
    val grid = parse(input)
    val possibleSteps =
      grid.map((coord, _) => (coord, neighbours(coord, grid).filter(c => grid.getOrElse(c, '#') != '#')))
    scenicRoute(grid, startOf(grid), endOf(grid), possibleSteps.mapValues(s => s.map((_, 1))).toMap)

  def remapGraph(grid: Map[Coord, Char], possibleSteps: Map[Coord, Seq[Coord]]) =
    val nodes = possibleSteps
      .filter(_._2.size > 2)
      .keySet + startOf(grid) + endOf(grid)

    case class Step(at: Coord, distance: Int)
    given Ordering[Step] = Ordering.by(_.distance)
    def fill(start: Coord, end: Coord) =
      val queue = mutable.PriorityQueue(Step(at = start, distance = 0))
      val seen = mutable.Set(start)
      while !queue.isEmpty && queue.head.at != end do
        val step = queue.dequeue
        queue ++= possibleSteps(step.at)
          .filter(seen.add)
          .filter(coord => coord == end || !nodes.contains(coord))
          .map(coord => Step(at = coord, distance = step.distance + 1))
      if queue.isEmpty then -1 else queue.head.distance

    def pairs[T](xs: Iterable[T]) =
      for (x, i) <- xs.iterator.zipWithIndex; (y, j) <- xs.iterator.zipWithIndex; if i != j yield (x, y)
  
    import org.lemon.advent.lib.graph.pathFind
    def adjacent(end: Coord)(coord: Coord) = possibleSteps(coord).filter(c => c == end || !nodes.contains(c))

    pairs(nodes)
      // .map((lhs, rhs) => (lhs, rhs, fill(lhs, rhs)))
      // .filter(_._3 != -1)
      .map((lhs, rhs) => (lhs, rhs, pathFind(adjacent(rhs), lhs, rhs).map(_.distance)))
      .filter(_._3.isDefined)
      .toSeq.groupBy(_._1)
      // .mapValues(edges => edges.map(_.tail))
      .mapValues(edges => edges.map(t => (t._2, t._3.get)))
      .toMap

  def part2(input: String) =
    val grid = parse(input)
    val possibleSteps = grid
      .filter(_._2 != '#')
      .map((coord, _) => (coord, coord.adjacent.filter(c => grid.getOrElse(c, '#') != '#')))
    scenicRoute(grid, startOf(grid), endOf(grid), remapGraph(grid, possibleSteps))
