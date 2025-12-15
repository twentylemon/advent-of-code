package org.lemon.advent.lib.`2d`

import Coord.*

object Direction:
  given Ordering[Direction] = Ordering.by(_.ordinal)

  def apply(ch: String | Char): Direction =
    unapply(ch).getOrElse(throw IllegalArgumentException(s"unknown direction character: $ch"))

  def unapply(ch: String | Char): Option[Direction] =
    ch match
      case "U" | 'U' | "u" | 'u' | "^" | '^' => Some(Up)
      case "D" | 'D' | "d" | 'd' | "v" | 'v' => Some(Down)
      case "L" | 'L' | "l" | 'l' | "<" | '<' => Some(Left)
      case "R" | 'R' | "r" | 'r' | ">" | '>' => Some(Right)
      case _ => None

/** A cardinal direction in 2d space.
  * @param unitVector the vector representing the direction
  */
enum Direction(val unitVector: Coord):
  case Up extends Direction(unitUp)
  case Left extends Direction(unitLeft)
  case Down extends Direction(unitDown)
  case Right extends Direction(unitRight)

  def turnLeft = Direction.fromOrdinal((ordinal + 1) % Direction.values.size)
  def turnAround = Direction.fromOrdinal((ordinal + 2) % Direction.values.size)
  def turnRight = Direction.fromOrdinal((ordinal + 3) % Direction.values.size)
