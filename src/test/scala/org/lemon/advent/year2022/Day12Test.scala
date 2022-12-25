package org.lemon.advent.year2022

import scala.collection.mutable
import org.lemon.advent._

class Day12Test extends UnitTest {

  type Coord = (Int, Int)
  extension (coord: Coord)
    def x = coord._1
    def y = coord._2
    def up = (coord.x, coord.y - 1)
    def down = (coord.x, coord.y + 1)
    def left = (coord.x - 1, coord.y)
    def right = (coord.x + 1, coord.y)
    def manhattan(rhs: Coord) = (coord.x - rhs.x).abs + (coord.y - rhs.y).abs

  type Maze = Seq[Seq[Char]]
  extension (maze: Maze)
    def apply(coord: Coord): Char =
      val c = maze(coord.y)(coord.x)
      c match
        case 'S' => 'a'
        case 'E' => 'z'
        case _ => c
    def isCell(coord: Coord) = maze.indices.contains(coord.y) && maze(coord.y).indices.contains(coord.x)
    def findFirstOf(c: Char) = (for
      y <- maze.indices
      x <- maze(y).indices
      if maze(y)(x) == c
    yield (x, y)).head
    def start = maze.findFirstOf('S')
    def end = maze.findFirstOf('E')
    def neighbours(coord: Coord) = Seq(coord.up, coord.down, coord.left, coord.right).filter(maze.isCell)

  def parseMaze(lines: Seq[String]) = lines.map(_.toSeq)

  private def asPath(start: Seq[Coord], end: Coord, cameFrom: mutable.Map[Coord, Coord]): Seq[Coord] =
    Iterator.iterate(end)(cameFrom).takeWhile(!start.contains(_)).toSeq

  def solve(maze: Maze)(start: Seq[Coord], end: Coord) =
    val stepsTaken = mutable.Map[Coord, Int](start.map(_ -> 0): _*)
    val queue = mutable.PriorityQueue[Coord]()(Ordering[Int].on((c: Coord) => c.manhattan(end) + stepsTaken(c)).reverse)

    val cameFrom = mutable.Map.empty[Coord, Coord]
    queue ++= start

    while !queue.isEmpty && queue.head != end do
      val cell = queue.dequeue
      val stepsToNeigh = stepsTaken(cell) + 1
      queue ++= maze.neighbours(cell)
        .filter(neigh => maze(neigh) - maze(cell) < 2)
        .filter(neigh => !stepsTaken.contains(neigh) || stepsTaken(neigh) > stepsToNeigh)
        .tapEach(neigh => stepsTaken.put(neigh, stepsToNeigh))
        .tapEach(neigh => cameFrom.put(neigh, cell))

    asPath(start, end, cameFrom)

  def part1(in: Seq[String]) =
    val maze = parseMaze(in)
    solve(maze)(Seq(maze.start), maze.end).size

  def part2(in: Seq[String]) =
    val maze = parseMaze(in)
    val starts =
      for
        y <- maze.indices
        x <- maze(y).indices
        if maze((x, y)) == 'a'
      yield (x, y)
    solve(maze)(starts, maze.end).size

  test("part 1 example") {
    val in = """|Sabqponm
                |abcryxxl
                |accszExk
                |acctuvwj
                |abdefghi""".stripMargin

    part1(in.linesIterator.toSeq) shouldBe 31
  }

  test("part 1") {
    part1(readLines(file(2022)(12))) shouldBe 534
  }

  test("part 2 example") {
    val in = """|Sabqponm
                |abcryxxl
                |accszExk
                |acctuvwj
                |abdefghi""".stripMargin

    part2(in.linesIterator.toSeq) shouldBe 29
  }

  test("part 2") {
    part2(readLines(file(2022)(12))) shouldBe 525
  }
}
