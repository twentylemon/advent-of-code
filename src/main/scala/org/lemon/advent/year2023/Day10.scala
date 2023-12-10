package org.lemon.advent.year2023

import scala.collection.mutable
import org.lemon.advent.Coord2._
import cats.instances.int

private object Day10:

  case class Pipe(char: Char, loc: Coord):
    val connections =
      char match
        case '|' => Seq(loc.up, loc.down)
        case '-' => Seq(loc.left, loc.right)
        case 'L' => Seq(loc.up, loc.right)
        case 'J' => Seq(loc.up, loc.left)
        case '7' => Seq(loc.down, loc.left)
        case 'F' => Seq(loc.down, loc.right)
        case '.' => Seq.empty[Coord]
        case 'S' => loc.adjacent
        case _ => Seq.empty[Coord]

  def parse(input: String) = input.linesIterator
    .zipWithIndex
    .flatMap((line, y) => line.zipWithIndex.map((c, x) => Pipe(char = c, loc = (x, y))))
    .map(pipe => pipe.loc -> pipe)
    .toMap

  def startOf(grid: Map[Coord, Pipe]) = grid.values.find(_.char == 'S').get

  def pathFind(start: Coord, grid: Map[Coord, Pipe]): Map[Pipe, Int] =
    val stepsTaken = mutable.Map(start -> 0)
    val queue =
      mutable.PriorityQueue[Coord]()(Ordering[Int].on((c: Coord) => c.manhattan(start) + stepsTaken(c)).reverse)

    queue += start
    while !queue.isEmpty && queue.size < 10 * grid.size do
      val cell = queue.dequeue
      val stepsToNeigh = stepsTaken(cell) + 1
      queue ++= grid(cell).connections
        .filter(grid.contains)
        .filter(neigh => grid(neigh).connections.contains(cell))
        .filter(neigh => stepsTaken.getOrElse(neigh, Int.MaxValue) > stepsToNeigh)
        .tapEach(neigh => stepsTaken.put(neigh, stepsToNeigh))

    stepsTaken.map((coord, steps) => (grid(coord), steps)).toMap

  def part1(input: String) =
    val grid = parse(input)
    val path = pathFind(startOf(grid).loc, grid)
    path.values.max

  def determinePipe(loc: Coord, grid: Map[Coord, Pipe]) =
    val connected = grid.values.filter(_.connections.contains(loc)).map(_.loc).toSet
    Seq('|', '-', 'L', 'J', '7', 'F')
      .map(Pipe(_, loc))
      .find(p => connected.subsetOf(p.connections.toSet))
      .get

  def rayCast(destination: Coord, path: Set[Coord], grid: Map[Coord, Pipe]) =
    enum BorderState:
      case Nil, Left, Right

    def nextBorder(loc: Coord, border: BorderState) =
      if !path.contains(loc) then BorderState.Nil
      else
        border match
          case BorderState.Nil =>
            grid(loc).char match
              case 'F' => BorderState.Left
              case '7' => BorderState.Right
              case _ => BorderState.Nil
          case BorderState.Left =>
            grid(loc).char match
              case 'L' | 'J' => BorderState.Nil
              case _ => BorderState.Left
          case BorderState.Right =>
            grid(loc).char match
              case 'J' | 'L' => BorderState.Nil
              case _ => BorderState.Right

    def intersectsPath(loc: Coord, border: BorderState) =
      if !path.contains(loc) then false
      else
        border match
          case BorderState.Nil => grid(loc).char == '-'
          case BorderState.Left => grid(loc).char == 'J'
          case BorderState.Right => grid(loc).char == 'L'

    val origin: Coord = (destination.x, 0)
    Iterator.unfold((origin, BorderState.Nil))((loc, border) =>
      if loc == destination then None
      else Some((if intersectsPath(loc, border) then 1 else 0, (loc.down, nextBorder(loc, border))))
    )
      .sum

  def part2(input: String) =
    val grid = parse(input)
    val start = startOf(grid)
    val path = pathFind(start.loc, grid).keySet.map(_.loc)
    val gridWithStartPipe = grid.updated(start.loc, determinePipe(start.loc, grid))
    grid.keysIterator
      .filterNot(path.contains)
      .map(rayCast(_, path, gridWithStartPipe))
      .count(_ % 2 == 1)
