package org.lemon.advent.year2016

import org.lemon.advent.lib.*
import org.lemon.advent.lib.graph.*

private object Day11:

  case class State(elevator: Int, things: Seq[(Int, Int)])

  def parse(input: String): State =
    val things = input.linesIterator.zipWithIndex
      .flatMap((line, i) =>
        line.wsv.sliding2.collect {
          case (element, s"generator$_") => (element, true) -> (i + 1)
          case (micro, s"microchip$_") => (micro.dropRight("-compatible".size), false) -> (i + 1)
        }
      ).toSeq
      .groupBy(_._1._1)
      .values
      .map(ele => (ele.find(_._1._2).get._2, ele.find(!_._1._2).get._2)) // pairs (gen, chip)
      .toSeq
      .sorted
    State(elevator = 1, things)

  def adjacency(state: State) =
    def valid(state: State) = state.things.forall((gen, chip) => gen == chip || !state.things.exists(_._1 == chip))
    val State(elevator, things) = state
    val here = things.zipWithIndex.flatMap { case ((gen, chip), i) =>
      Option.when(gen == elevator)((i, true)) ++ Option.when(chip == elevator)((i, false))
    }
    for
      bring <- here.combinations(1) ++ here.combinations(2)
      floor <- Seq(elevator + 1, elevator - 1).filter(e => e >= 1 && e <= 4)
      nextThings = things.zipWithIndex.map { case ((gen, chip), i) =>
        (if bring.contains((i, true)) then floor else gen, if bring.contains((i, false)) then floor else chip)
      }.sorted
      nextState = State(floor, nextThings)
      if valid(nextState)
    yield nextState

  def part1(input: String) =
    pathFind(
      adjacency = adjacency,
      start = parse(input),
      ends = _.things.forall((gen, chip) => gen == 4 && chip == 4),
    ).get.distance

  def part2(input: String) =
    val start = parse(input)
    pathFind(
      adjacency = adjacency,
      heuristic = _.things.map((gen, chip) => 4 - gen + 4 - chip).sum / 2,
      start = start.copy(things = (start.things ++ Seq((1, 1), (1, 1))).sorted),
      ends = _.things.forall((gen, chip) => gen == 4 && chip == 4),
    ).get.distance
