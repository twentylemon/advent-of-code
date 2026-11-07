package org.lemon.advent.lib.geometry

import Coord.*

object Direction:
  given Ordering[Direction] = Ordering.by(_.ordinal)

  private val lefts = values.map(d => fromOrdinal((d.ordinal + 1) % values.size))
  private val arounds = values.map(d => fromOrdinal((d.ordinal + 2) % values.size))
  private val rights = values.map(d => fromOrdinal((d.ordinal + 3) % values.size))

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

  def turnLeft = Direction.lefts(ordinal)
  def turnAround = Direction.arounds(ordinal)
  def turnRight = Direction.rights(ordinal)
