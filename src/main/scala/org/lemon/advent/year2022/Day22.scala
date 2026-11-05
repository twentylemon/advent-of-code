package org.lemon.advent.year2022

import org.lemon.advent.lib.*
import org.lemon.advent.lib.geometry.*
import org.lemon.advent.lib.graph.*
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
    val split = in.chunks
    val net = Coord.gridToMap(split(0)).filter(_._2 != ' ')
    val actions = parseActions(split(1))
    val size = math.sqrt(net.size / 6).toInt
    val cube = Cuboid(size, size, size)

    case class Walker(at: Point3[Int], orientation: Orientation):
      def face = (at, orientation.up)
      def step =
        val next = at.move(orientation)
        if cube.contains(next) then copy(at = next) else copy(orientation = orientation.turnDown)

    def turn(orientation: Orientation, from: Direction, to: Direction) =
      Iterator.iterate((from, orientation))((d, o) => (d.turnRight, o.turnRight)).find(_._1 == to).get._2

    case class Tile(coord: Coord, walker: Walker)
    def adjacent(tile: Tile) = Direction.values.filter(d => net.contains(tile.coord + d)).map(d =>
      val walker = tile.walker.copy(orientation = turn(tile.walker.orientation, Right, d)).step
      Tile(tile.coord + d, walker.copy(orientation = turn(walker.orientation, d, Right)))
    )

    val corner = net.keys.filter(_.y == 0).minBy(_.x)
    val tiles = fill(adjacent, Tile(corner, Walker(Point3.origin, Orientation.RightOut)))
    val byFace = tiles.map(tile => (tile.walker.face, tile)).toMap
    def blocked(walker: Walker) = net(byFace(walker.face).coord) == '#'

    val start = tiles.filter(tile => tile.coord.y == 0 && net(tile.coord) == '.').minBy(_.coord.x).walker
    val end = actions.foldLeft(start) {
      case (walker, TurnLeft) => walker.copy(orientation = walker.orientation.turnLeft)
      case (walker, TurnRight) => walker.copy(orientation = walker.orientation.turnRight)
      case (walker, Walk(steps)) => Iterator.iterate(walker)(_.step).take(steps + 1).takeWhile(!blocked(_)).last
    }

    val tile = byFace(end.face)
    val facing = Direction.values.find(d => turn(tile.walker.orientation, Right, d) == end.orientation).get
    1000 * (tile.coord.y + 1) + 4 * (tile.coord.x + 1) + facingValue(facing)
