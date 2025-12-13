package org.lemon.advent.year2022

import scala.collection.mutable

private object Day24:

  type Coord = (Int, Int)
  extension (coord: Coord)
    def x = coord._1
    def y = coord._2
    def up = (coord.x, coord.y - 1)
    def down = (coord.x, coord.y + 1)
    def left = (coord.x - 1, coord.y)
    def right = (coord.x + 1, coord.y)
    def cardinals = Seq(coord.up, coord.down, coord.left, coord.right)
    def manhattan(rhs: Coord) = (coord.x - rhs.x).abs + (coord.y - rhs.y).abs
    def +(rhs: Coord) = (coord.x + rhs.x, coord.y + rhs.y)

  case class Blizzard(at: Coord, direction: Coord)

  case class Maze(xBounds: Range, yBounds: Range, blizzards: Seq[Set[Coord]], start: Coord, end: Coord):
    def inBounds(coord: Coord) = coord match
      case c if c == start || c == end => true
      case c => xBounds.contains(c.x) && yBounds.contains(c.y)

  def cycle(n: Int, bounds: Range): Int =
    if n > bounds.max then bounds.min
    else if n < bounds.min then bounds.max
    else n

  def cycle(coord: Coord, xBounds: Range, yBounds: Range): Coord = (cycle(coord.x, xBounds), cycle(coord.y, yBounds))

  def move(blizzard: Blizzard, xBounds: Range, yBounds: Range) =
    blizzard.copy(at = cycle(blizzard.at + blizzard.direction, xBounds, yBounds))

  def parseBoard(in: Seq[String]) =
    val start = (in.head.indexOf('.'), 0)
    val end = (in.last.indexOf('.'), in.size - 1)
    val yBounds = 1 to in.size - 2
    val xBounds = 1 to in.head.size - 2
    val initialBlizzards =
      for
        y <- in.indices
        x <- in(y).indices
        if in(y)(x) != '.'
        if in(y)(x) != '#'
      yield in(y)(x) match
        case '>' => Blizzard(at = (x, y), direction = (1, 0))
        case '<' => Blizzard(at = (x, y), direction = (-1, 0))
        case '^' => Blizzard(at = (x, y), direction = (0, -1))
        case 'v' => Blizzard(at = (x, y), direction = (0, 1))

    def gcd(a: Int, b: Int): Int = if b == 0 then a else gcd(b, a % b)
    def lcm(a: Int, b: Int) = a * b / gcd(a, b)
    def tick(blizzards: Seq[Blizzard]) = blizzards.map(move(_, xBounds, yBounds))
    val allBlizzards = Iterator.iterate(initialBlizzards)(tick)
      .take(lcm(xBounds.size, yBounds.size))
      .map(_.map(_.at).toSet)
      .toIndexedSeq

    Maze(xBounds, yBounds, allBlizzards, start, end)

  case class Step(at: Coord, time: Int)
  def solve(maze: Maze, startingTime: Int = 0): Step =
    given Ordering[Step] = Ordering[Int].on((step: Step) => step.at.manhattan(maze.end) + step.time).reverse
    val queue = mutable.PriorityQueue[Step]()

    val visited = mutable.Set.empty[Step]
    queue += Step(at = maze.start, time = startingTime)

    while !queue.isEmpty && queue.head.at != maze.end do
      val step = queue.dequeue
      val time = step.time + 1
      val blizzards = maze.blizzards(time % maze.blizzards.size)
      val neighbours = (step.at.cardinals :+ step.at).filter(maze.inBounds).map(Step(_, time))

      queue ++= neighbours
        .filterNot(n => blizzards(n.at))
        .filterNot(visited)
        .tapEach(visited.add)

    queue.dequeue

  def part1(in: Seq[String]) =
    val maze = parseBoard(in)
    solve(maze).time

  def part2(in: Seq[String]) =
    val maze = parseBoard(in)
    val moveThoseDamnElves = solve(maze)
    val goBackCauseTheyJerks = solve(maze.copy(start = maze.end, end = maze.start), moveThoseDamnElves.time)
    val meetBackUpFfs = solve(maze, goBackCauseTheyJerks.time)
    meetBackUpFfs.time
