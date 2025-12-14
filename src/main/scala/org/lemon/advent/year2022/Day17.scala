package org.lemon.advent.year2022

import org.lemon.advent.lib.*
import org.lemon.advent.lib.`2d`.*

private object Day17:

  case class BoardState(board: Set[Coord], wind: String, cycle: Int, rock: Int, maxHeight: Int)

  val rocks: Seq[Seq[Coord]] = Seq(
    (0 to 3).map(Coord(_, 0)).toSeq, // wide boy
    Seq((1, 0), (0, 1), (1, 1), (2, 1), (1, 2)), // crossy lad
    Seq((0, 0), (1, 0), (2, 0), (2, 1), (2, 2)), // the knight moves like an L
    (0 to 3).map(Coord(0, _)).toSeq, // tall boy
    for x <- 0 to 1; y <- 0 to 1 yield Coord(x, y) // box boy
  )

  extension (rock: Seq[Coord])
    def translate(amount: Coord) = rock.map(_ + amount)

  def occupied(coord: Coord, board: Set[Coord]) = coord.x <= 0 || coord.x >= 8 || board(coord)

  def trickle(board: Set[Coord], rock: Seq[Coord], wind: String, cycle: Int): (Seq[Coord], Int) =
    val pushed = rock.translate(if wind(cycle) == '>' then (1, 0) else (-1, 0))
    val afterWind = if pushed.forall(!occupied(_, board)) then pushed else rock
    val fall = afterWind.translate((0, -1))
    val nextCycle = (cycle + 1) % wind.size
    if fall.exists(occupied(_, board)) then (afterWind, nextCycle) else trickle(board, fall, wind, nextCycle)

  def dropRock(state: BoardState) =
    val nextRock = rocks(state.rock).translate((3, state.maxHeight + 4))
    val (rest, cycle) = trickle(state.board, nextRock, state.wind, state.cycle)
    state.copy(
      board = state.board ++ rest,
      cycle = cycle,
      rock = (state.rock + 1) % rocks.size,
      maxHeight = state.maxHeight.max(rest.map(_.y).max)
    )

  def rocksFallEveryoneDies(wind: String) =
    val initial = BoardState(board = (0 to 7).map(Coord(_, 0)).toSet, wind = wind, cycle = 0, rock = 0, maxHeight = 0)
    Iterator.iterate(initial)(dropRock)

  def part1(input: String) =
    rocksFallEveryoneDies(input).nth(2022).maxHeight

  def part2(input: String) =
    def topProfile(s: BoardState): Seq[Int] =
      (1 to 7).map(x => s.maxHeight - s.board.filter(_.x == x).map(_.y).max)

    def sameState(s1: BoardState, s2: BoardState) =
      s1.cycle == s2.cycle && s1.rock == s2.rock && topProfile(s1) == topProfile(s2)

    val initial = BoardState(board = (0 to 7).map(Coord(_, 0)).toSet, wind = input, cycle = 0, rock = 0, maxHeight = 0)
    val cycle = findCycle(initial)(dropRock, sameState)

    val target = 1_000_000_000_000L
    val afterOneCycle = dropRock(cycle.history(cycle.offset + cycle.period - 1))
    val heightPerCycle = afterOneCycle.maxHeight - cycle.history(cycle.offset).maxHeight

    val posInCycle = ((target - cycle.offset) % cycle.period).toInt
    val numFullCycles = (target - cycle.offset) / cycle.period
    cycle.history(cycle.offset + posInCycle).maxHeight + numFullCycles * heightPerCycle
