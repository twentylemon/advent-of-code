package org.lemon.advent.year2022

import org.lemon.advent.lib.*
import org.lemon.advent.lib.`2d`.*
import scala.collection.mutable

private object Day22:

  enum Action:
    case TurnLeft, TurnRight
    case Walk(steps: Int)

  import Direction.*, Action.*

  def facingValue(d: Direction) =
    d match
      case Right => 0
      case Down => 1
      case Left => 2
      case Up => 3

  class Node(val location: Coord, val contents: Char):
    var east: Node = null
    var west: Node = null
    var north: Node = null
    var south: Node = null

    def neighbour(facing: Direction) =
      facing match
        case Right => east
        case Down => south
        case Left => west
        case Up => north

    def connect(facing: Direction, node: Node) =
      facing match
        case Right => node.west = this; this.east = node
        case Down => node.north = this; this.south = node
        case Left => node.east = this; this.west = node
        case Up => node.south = this; this.north = node

    def walk(facing: Direction): LazyList[Node] = LazyList.iterate(this)(_ `neighbour` facing)

    override def toString = s"Node(location=$location, contents=$contents)"

  def parseBoard(in: Seq[String]) =
    def parseRow(line: String, y: Int): Node =
      val nodes =
        line.zipWithIndex.filter((c, _) => !c.isWhitespace).map((c, x) =>
          Node(location = (x + 1, y + 1), contents = c)
        )
      (nodes :+ nodes.head).sliding2.foreach((a, b) => a.connect(Right, b))
      nodes.head

    val rows = in.zipWithIndex.map(parseRow)
    val width = in.map(_.size).max
    rows.foreach(topHead =>
      topHead.walk(Right).takeWhile(_.neighbour(Down) == null).foreach(top =>
        val bottom = (0 until rows.size).iterator
          .map(offset => rows((top.location.y + offset) % rows.size))
          .map(head => head.walk(Right).take(width).find(_.location.x == top.location.x))
          .flatten
          .next
        top.connect(Down, bottom)
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
          case s if s.startsWith("R") => TurnRight
          case s if s.startsWith("L") => TurnLeft
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

  case class State(at: Node, facing: Direction)

  def walk(state: State, action: Action): State = action match
    case TurnLeft => state.copy(facing = state.facing.turnLeft)
    case TurnRight => state.copy(facing = state.facing.turnRight)
    case Walk(steps) => state.copy(at = state.at.walk(state.facing).take(steps + 1).takeWhile(!isWall(_)).last)

  def part1(in: String) =
    val split = in.chunks
    val rows = parseBoard(split(0).linesIterator.toSeq)
    val actions = parseActions(split(1))

    val start = State(at = rows(0).walk(Right).dropWhile(isWall).head, facing = Right)
    val end = actions.foldLeft(start)(walk)

    1000 * end.at.location.y + 4 * end.at.location.x + facingValue(end.facing)

  def part2(in: String) =
    case class State(at: Coord, facing: Direction)

    val split = in.chunks
    val walls = Coord.gridToMap(split(0)).filter(_._2 == '#').keySet
    val actions = parseActions(split(1)) :+ null.asInstanceOf[Action]

    val startNodeLoc = parseBoard(split(0).linesIterator.toSeq)(0).walk(Right).dropWhile(isWall).head.location
    val start = State(at = (startNodeLoc.x - 1, startNodeLoc.y - 1), facing = Right)

    val end = actions.grouped(2).foldLeft(start)((state, moveAndTurn) =>
      val move = moveAndTurn(0).asInstanceOf[Walk]

      val nextState = (1 to move.steps).foldLeft(state)((it, _) =>
        val State(at, facing) = it
        val (faceX, faceY) = (at.x / 50, at.y / 50)
        val (modX, modY) = (at.x % 50, at.y % 50)
        val next = at + facing
        val nextX = if next.x > 0 then next.x / 50 else next.x
        val nextY = if next.y > 0 then next.y / 50 else next.y

        val (nextAt, nextFacing): (Coord, Direction) =
          (faceX, faceY, nextX, nextY) match
            case (a, b, c, d) if a == c && b == d => (next, facing) // staying on same face

            // _AB
            // _C_
            // ED_
            // F__
            case (1, 0, 2, 0) => (next, facing) // A -> B
            case (1, 0, 1, 1) => (next, facing) // A -> C
            case (1, 0, 0, 0) => (Coord(0, 149 - modY), Right) // A -> E
            case (1, 0, 1, -1) => (Coord(0, 150 + modX), Right) // A -> F

            case (2, 0, 1, 0) => (next, facing) // B -> A
            case (2, 0, 2, 1) => (Coord(99, 50 + modX), Left) // B -> C
            case (2, 0, 3, 0) => (Coord(99, 149 - modY), Left) // B -> D
            case (2, 0, 2, -1) => (Coord(modX, 199), facing) // B -> F

            case (1, 1, 1, 0) => (next, facing) // C -> A
            case (1, 1, 2, 1) => (Coord(100 + modY, 49), Up) // C -> B
            case (1, 1, 1, 2) => (next, facing) // C -> D
            case (1, 1, 0, 1) => (Coord(modY, 100), Down) // C -> E

            case (1, 2, 2, 2) => (Coord(149, 49 - modY), Left) // D -> B
            case (1, 2, 1, 1) => (next, facing) // D -> C
            case (1, 2, 0, 2) => (next, facing) // D -> E
            case (1, 2, 1, 3) => (Coord(49, 150 + modX), Left) // D -> F

            case (0, 2, -1, 2) => (Coord(50, 49 - modY), Right) // E -> A
            case (0, 2, 0, 1) => (Coord(50, 50 + modX), Right) // E -> C
            case (0, 2, 1, 2) => (next, facing) // E -> D
            case (0, 2, 0, 3) => (next, facing) // E -> F

            case (0, 3, -1, 3) => (Coord(50 + modY, 0), Down) // F -> A
            case (0, 3, 0, 4) => (Coord(100 + modX, 0), facing) // F -> B
            case (0, 3, 1, 3) => (Coord(50 + modY, 149), Up) // F -> D
            case (0, 3, 0, 2) => (next, facing) // F -> E
            case _ =>
              throw new IllegalStateException(s"unexpected face transition: ($faceX, $faceY) -> ($nextX, $nextY)")

        if walls(nextAt) then it else State(at = nextAt, facing = nextFacing)
      )

      moveAndTurn(1) match
        case TurnLeft => nextState.copy(facing = nextState.facing.turnLeft)
        case TurnRight => nextState.copy(facing = nextState.facing.turnRight)
        case _ => nextState
    )
    1000 * (end.at.y + 1) + 4 * (end.at.x + 1) + facingValue(end.facing)
