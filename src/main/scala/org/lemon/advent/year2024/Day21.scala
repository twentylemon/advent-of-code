package org.lemon.advent.year2024

import org.lemon.advent.lib.*
import org.lemon.advent.lib.`2d`.*
import org.lemon.advent.lib.graph.*

private object Day21:

  def parse(input: String) = input.linesIterator.toSeq

  def directional = IndexedSeq(
    " ^A".toSeq,
    "<v>".toSeq,
  )
  def directionalMap = asMap(directional)
  def keypad = IndexedSeq(
    "789".toSeq,
    "456".toSeq,
    "123".toSeq,
    " 0A".toSeq,
  )
  def keypadMap = asMap(keypad)

  def asMap(buttons: Seq[Seq[Char]]): Map[Char, Coord] =
    Area(buttons).filter(buttons(_) != ' ').map(coord => buttons(coord) -> coord).toMap

  def pathsBetween(buttons: Seq[Seq[Char]])(start: Char, end: Char) =
    def adjacency(coord: Coord): Seq[Coord] = coord.adjacent.filter(buttons.hasCoord).filter(buttons(_) != ' ')
    val map = asMap(buttons)
    val fin = map(end)
    allShortestPaths(adjacency, map(start), _ == fin)
      .map(path => path.path.map(buttons(_)).mkString)

  def asNested(map: Map[Char, Coord])(path: String) =
    'A' +: path.zip(path.tail).map((from, to) =>
      map(from).directionTo(map(to)) match
        case Some(Direction.Up) => '^'
        case Some(Direction.Down) => 'v'
        case Some(Direction.Left) => '<'
        case Some(Direction.Right) => '>'
        case _ => throw AssertionError(s"no direction from $from to $to")
    ).mkString :+ 'A'

  def pathLength(code: String, totalBots: Int): Long = pathLength(code, totalBots, totalBots)
  def pathLength(code: String, totalBots: Int, arrowBots: Int): Long =
    lazy val length: (Char, Char, Int) => Long = memoize {
      case (from, to, 0) => pathsBetween(directional)(from, to).map(_.size).min
      case (from, to, depth) =>
        val board = if depth == totalBots then keypad else directional
        pathsBetween(board)(from, to)
          .map(asNested(asMap(board)))
          .map(nested =>
            nested.zip(nested.tail)
              .map((start, end) => length(start, end, depth - 1))
              .sum
          ).min
    }
    ('A' +: code).zip(code).map((start, end) => length(start, end, arrowBots)).sum

  def complexity(code: String, distance: Long) = code.filter(_.isDigit).toInt * distance

  def part1(input: String) =
    parse(input)
      .map(code => complexity(code, pathLength(code, 2)))
      .sum

  def part2(input: String) =
    parse(input)
      .map(code => complexity(code, pathLength(code, 25)))
      .sum

  // GARBO
  case class Node(
      depressurized: Robit, // at the keypad
      radiated: Robit, // at the directional pad controlling depressurized
      cold: Robit, // at the directional pad controlling radiated
      pressed: String = "",
  ):
    override def toString(): String = s"Node(d=$depressurized, r=$radiated, c=$cold, $pressed)"
  case class Robit(armLocation: Coord, buttons: Seq[Seq[Char]], pushed: String = ""):
    def pointingAt = buttons(armLocation)
    def wouldBePointingAt(coord: Coord) = Option.when(buttons.hasCoord(coord))(buttons(coord))
    override def toString(): String = s"Robit($pointingAt, $pushed)"

  def adjacency(code: String)(node: Node): Seq[Node] =
    val Node(depressurized, radiated, cold, _) = node
    // we can push any of the direction buttons, which will move cold's arm
    val directionalButtons = Direction.values
      .map(cold.armLocation + _)
      .filter(cold.wouldBePointingAt(_).exists(_ != ' '))
      .map(newLoc =>
        node.copy(
          cold = cold.copy(armLocation = newLoc),
          pressed =
            cold.armLocation.directionTo(newLoc).get match
              case Direction.Up => "^"
              case Direction.Down => "v"
              case Direction.Left => "<"
              case Direction.Right => ">"
        )
      )
      .toSeq

    // we can push A, which will push cold's active button
    val a = cold.pointingAt match
      case ' ' => None // pointing at a gap, cannot be pushed
      case 'A' => // if cold is gonna push A, radiated will push it's button
        radiated.pointingAt match
          case ' ' => None // pointing at a gap, cannot be pushed
          case 'A' => // radiated is pushing A, so depressurized will push it's button
            depressurized.pointingAt match
              case ' ' => None // pointing at a gap, cannot be pushed
              case key => // depressurized is pushing a button, so we can push it
                val newPushed = depressurized.pushed + key
                Option.when(code.startsWith(newPushed))(
                  node.copy(depressurized = depressurized.copy(pushed = newPushed), pressed = "A"),
                )
          case radiatedDir => // radiated is pointing at a direction, so it will move depressurized's arm
            val newLoc = depressurized.armLocation + Direction(radiatedDir)
            depressurized.wouldBePointingAt(newLoc).filter(_ != ' ')
              .map(_ =>
                node.copy(depressurized = depressurized.copy(armLocation = newLoc), pressed = "A")
              )
      case coldDir => // cold is pointing at a direction, so it will move radiated's arm
        val newLoc = radiated.armLocation + Direction(coldDir)
        radiated.wouldBePointingAt(newLoc).filter(_ != ' ')
          .map(_ =>
            node.copy(radiated = radiated.copy(armLocation = newLoc), pressed = "A")
          )
    directionalButtons ++ a.toSeq

  def route(code: String) =
    val start = Node(
      depressurized = Robit(Coord(2, 3), keypad),
      radiated = Robit(Coord(2, 0), directional),
      cold = Robit(Coord(2, 0), directional),
    )
    pathFind(adjacency(code), start, _.depressurized.pushed == code)
