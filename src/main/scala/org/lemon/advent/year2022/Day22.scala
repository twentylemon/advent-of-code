package org.lemon.advent.year2022

import org.lemon.advent.lib.*
import org.lemon.advent.lib.geometry.*
import org.lemon.advent.lib.graph.*

private object Day22:

  enum Action:
    case TurnLeft, TurnRight
    case Walk(steps: Int)

  import Direction.*, Action.*

  type NetState = (Coord, Direction)
  type CubeState = (Point3[Int], Orientation)

  def parseActions(path: String): Seq[Action] = """\d+|[LR]""".r.findAllIn(path).map {
    case "L" => TurnLeft
    case "R" => TurnRight
    case steps => Walk(steps.toInt)
  }.toSeq

  def parse(in: String): (Map[Coord, Char], Seq[Action]) =
    val Seq(board, path) = in.chunks
    (Coord.gridToMap(board).filter(_._2 != ' '), parseActions(path))

  def start(board: Map[Coord, Char]): NetState =
    (board.keys.filter(coord => coord.y == 0 && board(coord) == '.').minBy(_.x), Right)

  def facingValue(d: Direction) =
    d match
      case Right => 0
      case Down => 1
      case Left => 2
      case Up => 3

  def password(state: NetState) =
    val (at, facing) = state
    1000 * (at.y + 1) + 4 * (at.x + 1) + facingValue(facing)

  def wrap(board: Map[Coord, Char], facing: Direction)(at: Coord): Coord =
    val next = at + facing
    if board.contains(next) then next else at.walk(facing.turnAround).takeWhile(board.contains).last

  def walk(board: Map[Coord, Char])(state: NetState, action: Action): NetState =
    val (at, facing) = state
    action match
      case TurnLeft => (at, facing.turnLeft)
      case TurnRight => (at, facing.turnRight)
      case Walk(steps) =>
        (Iterator.iterate(at)(wrap(board, facing)).take(steps + 1).takeWhile(board(_) != '#').last, facing)

  def part1(in: String) =
    val (board, actions) = parse(in)
    password(actions.foldLeft(start(board))(walk(board)))

  def step(cube: Cuboid[Int])(state: CubeState): CubeState =
    val (at, facing) = state
    val next = at + facing
    if cube.contains(next - facing.up) then (next, facing) else (next - facing.up, facing.turnDown)

  def fold(net: Map[Coord, Char], cube: Cuboid[Int]): Map[CubeState, NetState] =
    def adjacency(state: (NetState, CubeState)) =
      val ((coord, dir), (at, facing)) = state
      val turns = Seq(((coord, dir.turnLeft), (at, facing.turnLeft)), ((coord, dir.turnRight), (at, facing.turnRight)))
      val forward = Option.when(net.contains(coord + dir))(((coord + dir, dir), step(cube)((at, facing))))
      turns ++ forward
    val corner = net.keys.filter(_.y == 0).minBy(_.x)
    fill(adjacency, ((corner, Right), (cube.topLeftFront.out, Orientation.RightOut))).map(_.swap).toMap

  def walkCube(cube: Cuboid[Int], surface: Map[Point3[Int], Char])(state: CubeState, action: Action): CubeState =
    val (at, facing) = state
    action match
      case TurnLeft => (at, facing.turnLeft)
      case TurnRight => (at, facing.turnRight)
      case Walk(steps) =>
        Iterator.iterate(state)(step(cube)).take(steps + 1).takeWhile((p, _) => surface(p) != '#').last

  def part2(in: String) =
    val (net, actions) = parse(in)
    val size = math.sqrt(net.size / 6).toInt
    val cube = Cuboid(size, size, size)

    val onNet = fold(net, cube)
    val onCube = onNet.map(_.swap)
    val surface = onNet.map { case ((at, _), (coord, _)) => (at, net(coord)) }

    password(onNet(actions.foldLeft(onCube(start(net)))(walkCube(cube, surface))))
