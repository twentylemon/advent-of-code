package org.lemon.advent.year2024

import org.lemon.advent.lib.*
import org.lemon.advent.lib.`2d`.*

private object Day25:

  def parse(input: String) =
    input.chunks.map(Coord.gridToMap)

  def part1(input: String) =
    val grids = parse(input)
    val locks = grids.filter(_(Coord(0, 0)) == '#')
    val keys = grids.filterNot(_(Coord(0, 0)) == '#')
    locks.cartesianProduct(keys)
      .count((lock, key) => lock.forall((coord, cell) => cell != '#' || key(coord) != '#'))
