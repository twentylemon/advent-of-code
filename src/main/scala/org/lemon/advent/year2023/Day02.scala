package org.lemon.advent.year2023

private object Day02:

  case class Dice(num: Int, colour: String)
  case class Game(id: Int, pulls: Seq[Seq[Dice]])

  def parsePulls(pulls: String): Seq[Seq[Dice]] = pulls.split(";")
    .map(_.split(","))
    .map(_.map(x => x.strip))
    .map(_.map(dice =>
      dice match
        case s"$num $colour" => Dice(num.toInt, colour)
    ))
    .map(_.toSeq)
    .toSeq

  def parse(input: String): Seq[Game] = input.linesIterator
    .map(_ match
      case s"Game $id: $games" => Game(id.toInt, parsePulls(games))
    )
    .toSeq

  def getMaxPerColour(game: Game) = game.pulls
    .flatten
    .groupBy(_.colour)
    .mapValues(_.maxBy(dice => dice.num))
    .mapValues(_.num)

  def isPossible(game: Game): Boolean =
    val max = getMaxPerColour(game)
    max("red") <= 12 && max("green") <= 13 && max("blue") <= 14

  def part1(input: String) = parse(input)
    .filter(isPossible)
    .map(_.id)
    .sum

  def part2(input: String) = parse(input)
    .map(getMaxPerColour)
    .map(_.values)
    .map(_.product)
    .sum
