package org.lemon.advent.year2022

import org.lemon.advent._

class Day17Test extends UnitTest {

  type Coord = (Int, Int)
  extension (coord: Coord)
    def x = coord._1
    def y = coord._2

  case class BoardState(board: Set[Coord], wind: String, cycle: Int, rock: Int, maxHeight: Int)

  val rocks = Seq(
    (0 to 3).map((_, 0)).toSeq, // wide boy
    Seq((1, 0), (0, 1), (1, 1), (2, 1), (1, 2)), // crossy lad
    Seq((0, 0), (1, 0), (2, 0), (2, 1), (2, 2)), // the knight moves like an L
    (0 to 3).map((0, _)).toSeq, // tall boy
    for x <- 0 to 1; y <- 0 to 1 yield (x, y) // box boy
  )

  extension (rock: Seq[Coord])
    def translate(amount: Coord) = rock.map(r => (r.x + amount.x, r.y + amount.y))

  def occupied(coord: Coord, board: Set[Coord]) = coord.x <= 0 || coord.x >= 8 || board(coord)

  def trickle(board: Set[Coord], rock: Seq[Coord], wind: String, cycle: Int): (Seq[Coord], Int) =
    val pushed = rock.translate(if wind(cycle) == '>' then (1, 0) else (-1, 0))
    val afterWind = if pushed.forall(!occupied(_, board)) then pushed else rock
    val fall = afterWind.translate((0, -1))
    val nextCycle = (cycle + 1) % wind.size
    if fall.exists(occupied(_, board)) then (afterWind, nextCycle) else trickle(board, fall, wind, nextCycle)

  def dropRock(state: BoardState) =
    val nextRock = rocks(state.rock).translate(3, state.maxHeight + 4)
    val (rest, cycle) = trickle(state.board, nextRock, state.wind, state.cycle)
    state.copy(
      board = state.board ++ rest,
      cycle = cycle,
      rock = (state.rock + 1) % rocks.size,
      maxHeight = state.maxHeight.max(rest.map(_.y).max)
    )

  def rocksFallEveryoneDies(wind: String) =
    val initial = BoardState(board = (0 to 7).map((_, 0)).toSet, wind = wind, cycle = 0, rock = 0, maxHeight = 0)
    Iterator.iterate(initial)(dropRock)

  def part1(wind: String) =
    rocksFallEveryoneDies(wind).drop(2022).next.maxHeight

  def part2(wind: String) =
    val cycleLength = rocks.size * wind.size // could be shorter, but the real cycle should go evenly into this
    val heights = rocksFallEveryoneDies(wind).map(_.maxHeight).take(2 * cycleLength).toSeq
    val heightChanges = heights.sliding(2).map(x => x(1) - x(0)).toSeq

    val cycleOfHeights = heightChanges.takeRight(cycleLength)
    val cycleStart = heightChanges.indexOfSlice(cycleOfHeights)
    val cycleHeightChange = heights(heightChanges.size - cycleLength) - heights(cycleStart)
    val cycleNumRocks = heightChanges.size - cycleLength - cycleStart

    val lastCycleStart = 1000000000000L - cycleStart
    val (numCycles, remaining) = (lastCycleStart / cycleNumRocks, lastCycleStart % cycleNumRocks)
    numCycles * cycleHeightChange + heights(cycleStart + remaining.toInt)

  test("part 1 example") {
    val in = ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"
    part1(in) shouldBe 3068
  }

  test("part 1") {
    part1(read(file(2022)(17))) shouldBe 3232
  }

  test("part 2 example") {
    val in = ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"
    part2(in) shouldBe 1514285714288L
  }

  test("part 2") {
    part2(read(file(2022)(17))) shouldBe 1585632183915L
  }
}
