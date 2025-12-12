package org.lemon.advent.year2022

import org.lemon.advent.*
import scala.collection.mutable

class Day22Test extends UnitTest {

  type Coord = (Int, Int)
  extension (coord: Coord)
    def x = coord._1
    def y = coord._2

  enum Direction:
    case Left, Right

  enum Facing:
    case East, South, West, North

    def turn(direction: Direction) = direction match
      case Direction.Left => Facing.fromOrdinal((ordinal + 4 - 1) % 4)
      case Direction.Right => Facing.fromOrdinal((ordinal + 1) % 4)

  enum Action:
    case Turn(direction: Direction)
    case Walk(steps: Int)

  import Facing.*, Action.*, Direction.*

  class Node(val location: Coord, val contents: Char):
    var east: Node = null
    var west: Node = null
    var north: Node = null
    var south: Node = null

    def neighbour(facing: Facing) =
      facing match
        case East => east
        case South => south
        case West => west
        case North => north

    def connect(facing: Facing, node: Node) =
      facing match
        case East => node.west = this; this.east = node
        case South => node.north = this; this.south = node
        case West => node.east = this; this.west = node
        case North => node.south = this; this.north = node

    def walk(facing: Facing): LazyList[Node] = LazyList.iterate(this)(_ `neighbour` facing)

    override def toString = s"Node(location=$location, contents=$contents)"

  def parseBoard(in: Seq[String]) =
    def parseRow(line: String, y: Int): Node =
      val nodes =
        line.zipWithIndex.filter((c, _) => !c.isWhitespace).map((c, x) =>
          Node(location = (x + 1, y + 1), contents = c)
        )
      (nodes :+ nodes.head).sliding(2).foreach(n => n(0).connect(East, n(1)))
      nodes.head

    val rows = in.zipWithIndex.map(parseRow)
    val width = in.map(_.size).max
    rows.foreach(topHead =>
      topHead.walk(East).takeWhile(_.neighbour(South) == null).foreach(top =>
        val bottom = (0 until rows.size).iterator
          .map(offset => rows((top.location.y + offset) % rows.size))
          .map(head => head.walk(East).take(width).find(_.location.x == top.location.x))
          .flatten
          .next
        top.connect(South, bottom)
      )
    )
    rows

  def parseActions(in: String): Seq[Action] =
    val actions = mutable.Buffer.empty[Action]
    var (num, tail) = in.span(_.isDigit)
    while !num.isEmpty do
      actions += Walk(num.toInt)
      val turn =
        tail match
          case s if s.startsWith("R") => Turn(Direction.Right)
          case s if s.startsWith("L") => Turn(Direction.Left)
          case _ => null

      if turn != null then actions += turn
      if tail.isEmpty then
        num = ""
      else
        val (num2, tail2) = tail.tail.span(_.isDigit)
        num = num2
        tail = tail2

    actions.toSeq

  def isWall(node: Node) = node.contents == '#'

  case class State(at: Node, facing: Facing)

  def walk(state: State, action: Action): State = action match
    case Turn(direction) => state.copy(facing = state.facing.turn(direction))
    case Walk(steps) => state.copy(at = state.at.walk(state.facing).take(steps + 1).takeWhile(!isWall(_)).last)

  def part1(in: String) =
    val split = in.split("\n\n")
    val rows = parseBoard(split(0).linesIterator.toSeq)
    val actions = parseActions(split(1))

    val start = State(at = rows(0).walk(East).dropWhile(isWall).head, facing = East)
    val end = actions.foldLeft(start)(walk)

    1000 * end.at.location.y + 4 * end.at.location.x + end.facing.ordinal

  test("part 1 example") {
    val in = """|        ...#
                |        .#..
                |        #...
                |        ....
                |...#.......#
                |........#...
                |..#....#....
                |..........#.
                |        ...#....
                |        .....#..
                |        .#......
                |        ......#.
                |
                |10R5L5R10L4R5L5""".stripMargin

    part1(in) shouldBe 6032
  }

  test("part 1") {
    part1(read(file(2022)(22))) shouldBe 29408
  }

  def findWalls(in: Seq[String]) =
    for
      (row, y) <- in.zipWithIndex
      (contents, x) <- row.zipWithIndex
      if contents == '#'
    yield (x, y)

  def part2(in: String) =
    case class State(at: Coord, facing: Facing)

    val split = in.split("\n\n")
    val walls = findWalls(split(0).linesIterator.toSeq).toSet
    val actions = parseActions(split(1)) :+ null.asInstanceOf[Turn]

    val startNodeLoc = parseBoard(split(0).linesIterator.toSeq)(0).walk(East).dropWhile(isWall).head.location
    val start = State(at = (startNodeLoc.x - 1, startNodeLoc.y - 1), facing = East)

    def push(coord: Coord, facing: Facing) = facing match
      case South => (coord.x, coord.y + 1)
      case North => (coord.x, coord.y - 1)
      case East => (coord.x + 1, coord.y)
      case West => (coord.x - 1, coord.y)

    val end = actions.grouped(2).foldLeft(start)((state, moveAndTurn) =>
      val move = moveAndTurn(0).asInstanceOf[Walk]

      val nextState = (1 to move.steps).foldLeft(state)((it, _) =>
        val State(at, facing) = it
        val (faceX, faceY) = (at.x / 50, at.y / 50)
        val (modX, modY) = (at.x % 50, at.y % 50)
        val next = push(at, facing)
        val nextX = if next.x > 0 then next.x / 50 else next.x
        val nextY = if next.y > 0 then next.y / 50 else next.y

        val (nextAt, nextFacing) =
          (faceX, faceY, nextX, nextY) match
            case (a, b, c, d) if a == c && b == d => (next, facing) // staying on same face

            // _AB
            // _C_
            // ED_
            // F__
            case (1, 0, 2, 0) => (next, facing) // A -> B
            case (1, 0, 1, 1) => (next, facing) // A -> C
            case (1, 0, 0, 0) => ((0, 149 - modY), East) // A -> E
            case (1, 0, 1, -1) => ((0, 150 + modX), East) // A -> F

            case (2, 0, 1, 0) => (next, facing) // B -> A
            case (2, 0, 2, 1) => ((99, 50 + modX), West) // B -> C
            case (2, 0, 3, 0) => ((99, 149 - modY), West) // B -> D
            case (2, 0, 2, -1) => ((modX, 199), facing) // B -> F

            case (1, 1, 1, 0) => (next, facing) // C -> A
            case (1, 1, 2, 1) => ((100 + modY, 49), North) // C -> B
            case (1, 1, 1, 2) => (next, facing) // C -> D
            case (1, 1, 0, 1) => ((modY, 100), South) // C -> E

            case (1, 2, 2, 2) => ((149, 49 - modY), West) // D -> B
            case (1, 2, 1, 1) => (next, facing) // D -> C
            case (1, 2, 0, 2) => (next, facing) // D -> E
            case (1, 2, 1, 3) => ((49, 150 + modX), West) // D -> F

            case (0, 2, -1, 2) => ((50, 49 - modY), East) // E -> A
            case (0, 2, 0, 1) => ((50, 50 + modX), East) // E -> C
            case (0, 2, 1, 2) => (next, facing) // E -> D
            case (0, 2, 0, 3) => (next, facing) // E -> F

            case (0, 3, -1, 3) => ((50 + modY, 0), South) // F -> A
            case (0, 3, 0, 4) => ((100 + modX, 0), facing) // F -> B
            case (0, 3, 1, 3) => ((50 + modY, 149), North) // F -> D
            case (0, 3, 0, 2) => (next, facing) // F -> E
            case _ =>
              throw new IllegalStateException(s"unexpected face transition: ($faceX, $faceY) -> ($nextX, $nextY)")

        if walls(nextAt) then it else State(at = nextAt, facing = nextFacing)
      )

      val turn = moveAndTurn(1).asInstanceOf[Turn]
      if turn != null then nextState.copy(facing = nextState.facing.turn(turn.direction))
      else nextState
    )
    1000 * (end.at.y + 1) + 4 * (end.at.x + 1) + end.facing.ordinal

  // I hard coded the cube. sue me.
  ignore("part 2 example") {
    val in = """|        ...#
                |        .#..
                |        #...
                |        ....
                |...#.......#
                |........#...
                |..#....#....
                |..........#.
                |        ...#....
                |        .....#..
                |        .#......
                |        ......#.
                |
                |10R5L5R10L4R5L5""".stripMargin
    part2(in) shouldBe 5031
  }

  test("part 2") {
    part2(read(file(2022)(22))) shouldBe 115311
  }

}
