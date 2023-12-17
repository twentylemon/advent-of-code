package org.lemon.advent.lib

import org.lemon.advent.lib.Coord2._

enum Direction(val unitVector: Coord):
  case Up extends Direction(unitUp)
  case Left extends Direction(unitLeft)
  case Down extends Direction(unitDown)
  case Right extends Direction(unitRight)

  def turnLeft = Direction.fromOrdinal((ordinal + 1) % Direction.values.size)
  def turnAround = Direction.fromOrdinal((ordinal + 2) % Direction.values.size)
  def turnRight = Direction.fromOrdinal((ordinal + 3) % Direction.values.size)
