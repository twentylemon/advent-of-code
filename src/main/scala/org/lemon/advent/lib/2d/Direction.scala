package org.lemon.advent.lib.`2d`

import Coord._

object Direction:

  given Ordering[Direction] = Ordering.by(_.ordinal)

  def apply(ch: String | Char): Direction = ch match
    case "U" | 'U' | "u" | 'u' | "^" | '^' => Up
    case "D" | 'D' | "d" | 'd' | "v" | 'v' => Down
    case "L" | 'L' | "l" | 'l' | "<" | '<' => Left
    case "R" | 'R' | "r" | 'r' | ">" | '>' => Right
    case _ => throw AssertionError(s"unknown direction character:  $ch")

enum Direction(val unitVector: Coord):
  case Up extends Direction(unitUp)
  case Left extends Direction(unitLeft)
  case Down extends Direction(unitDown)
  case Right extends Direction(unitRight)

  def turnLeft = Direction.fromOrdinal((ordinal + 1) % Direction.values.size)
  def turnAround = Direction.fromOrdinal((ordinal + 2) % Direction.values.size)
  def turnRight = Direction.fromOrdinal((ordinal + 3) % Direction.values.size)
