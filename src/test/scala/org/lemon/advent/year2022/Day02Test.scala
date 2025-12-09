package org.lemon.advent.year2022

import org.lemon.advent.*

class Day02Test extends UnitTest {
  sealed trait Toss
  case object Rock extends Toss
  case object Paper extends Toss
  case object Scissors extends Toss

  case class Game(me: Toss, them: Toss)

  def parseToss(c: Char) =
    c match
      case 'A' | 'X' => Rock
      case 'B' | 'Y' => Paper
      case 'C' | 'Z' => Scissors

  def parseGame(s: String) =
    val row = s.split(" ").map(_.head).map(parseToss)
    Game(them = row(0), me = row(1))

  def score(game: Game) = scoreToss(game) + scoreWin(game)

  def scoreToss(game: Game) =
    game match
      case Game(Rock, _) => 1
      case Game(Paper, _) => 2
      case Game(Scissors, _) => 3

  def scoreWin(game: Game) =
    game match
      case Game(x, y) if x == y => 3
      case Game(Rock, Scissors) | Game(Paper, Rock) | Game(Scissors, Paper) => 6
      case _ => 0

  def totalScore(input: String) = input.linesIterator.map(parseGame).map(score).sum

  sealed trait Outcome
  case object Win extends Outcome
  case object Lose extends Outcome
  case object Draw extends Outcome

  def parseOutcome(c: Char) =
    c match
      case 'X' => Lose
      case 'Y' => Draw
      case 'Z' => Win

  case class Strat(them: Toss, outcome: Outcome)

  given Conversion[Strat, Game] = _ match
    case Strat(x, Draw) => Game(x, x)
    case Strat(x, Lose) =>
      Game(them = x, me = Seq(Rock, Paper, Scissors).find(t => scoreWin(Game(me = t, them = x)) == 0).get)
    case Strat(x, Win) =>
      Game(them = x, me = Seq(Rock, Paper, Scissors).find(t => scoreWin(Game(me = t, them = x)) == 6).get)

  def parseStrat(s: String) =
    val row = s.split(" ").map(_.head)
    Strat(them = parseToss(row(0)), outcome = parseOutcome(row(1)))

  def totalScoreStrat(input: String) = input.linesIterator.map(parseStrat).map(x => score(x)).sum

  test("part 1 example") {
    val input = """
            |A Y
            |B X
            |C Z
            |""".stripMargin.strip

    totalScore(input) shouldBe 15
  }

  test("part 1") {
    totalScore(read(file(2022)(2))) shouldBe 10718
  }

  test("part 2 example") {
    val input = """
            |A Y
            |B X
            |C Z
            |""".stripMargin.strip

    totalScoreStrat(input) shouldBe 12
  }

  test("part 2") {
    totalScoreStrat(read(file(2022)(2))) shouldBe 14652
  }
}
