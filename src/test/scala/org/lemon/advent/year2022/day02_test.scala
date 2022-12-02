package org.lemon.advent.year2022

import org.lemon.UnitTest
import scala.util.Using
import scala.io.Source
import org.scalatest.Outcome
import scala.Conversion

private sealed trait Toss
private case object Rock extends Toss
private case object Paper extends Toss
private case object Scissors extends Toss

private case class Game(me: Toss, them: Toss)

private def parseToss(c: Char) = c match
    case 'A' | 'X' => Rock
    case 'B' | 'Y' => Paper
    case 'C' | 'Z' => Scissors

private def parseGame(s: String) =
    val row = s.split(" ").map(_.head).map(parseToss)
    Game(them = row(0), me = row(1))

private def score(game: Game) = scoreToss(game) + scoreWin(game)

private def scoreToss(game: Game) = game match
    case Game(Rock, _) => 1
    case Game(Paper, _) => 2
    case Game(Scissors, _) => 3

private def scoreWin(game: Game) = game match
    case Game(x, y) if x == y => 3
    case Game(Rock, Scissors) | Game(Paper, Rock) | Game(Scissors, Paper) => 6
    case _ => 0

private def totalScore(input: String) = input.linesIterator.map(parseGame).map(score).sum


private sealed trait Outcome
private case object Win extends Outcome
private case object Lose extends Outcome
private case object Draw extends Outcome

private def parseOutcome(c: Char) = c match
    case 'X' => Lose
    case 'Y' => Draw
    case 'Z' => Win

private case class Strat(them: Toss, outcome: Outcome)

given Conversion[Strat, Game] with
    def apply(strat: Strat): Game = strat match
        case Strat(x, Draw) => Game(x, x)
        case Strat(x, Lose) => Game(them=x, me=Seq(Rock, Paper, Scissors).find(t => scoreWin(Game(me=t, them=x)) == 0).get)
        case Strat(x, Win) => Game(them=x, me=Seq(Rock, Paper, Scissors).find(t => scoreWin(Game(me=t, them=x)) == 6).get)

private def parseStrat(s: String) =
    val row = s.split(" ").map(_.head)
    Strat(them = parseToss(row(0)), outcome = parseOutcome(row(1)))

private def totalScoreStrat(input: String) = input.linesIterator.map(parseStrat).map(x => score(x)).sum

class Day02Test extends UnitTest {

    test("part 1 example") {
        val input = """
            |A Y
            |B X
            |C Z
            |""".stripMargin.strip

        totalScore(input) shouldBe 15
    }

    test("part 1") {
        Using.resource(Source.fromResource("year2022/day02.txt"))(source => totalScore(source.mkString) shouldBe 10718)
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
        Using.resource(Source.fromResource("year2022/day02.txt"))(source => totalScoreStrat(source.mkString) shouldBe 14652)
    }
}