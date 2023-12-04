package org.lemon.advent.year2023

private object Day04:

  case class Card(id: Int, winning: Seq[Int], pulls: Seq[Int]):
    lazy val matches = winning intersect pulls

  def parseCard(line: String) =
    def asIntSeq(nums: String) = nums.split("\\s+").map(_.trim).filter(_.nonEmpty).map(_.toInt)
    line match
      case s"Card $n: $win | $pull" => Card(id = n.trim.toInt, winning = asIntSeq(win), pulls = asIntSeq(pull))

  def parse(input: String) = input.linesIterator
    .map(parseCard)

  def score(card: Card) =
    if card.matches.isEmpty then 0 else math.pow(2, card.matches.length - 1).toInt

  def part1(input: String) = parse(input)
    .map(score)
    .sum

  def winMe(pool: Iterable[Card], draws: Iterable[Card], depth: Int = 0): Int =
    def copyWinnings(from: Card) = pool
      .filter(_.id > from.id)
      .filter(_.id <= from.id + from.matches.length)

    val winnings = draws.flatMap(copyWinnings)
    if winnings.isEmpty then 0 else winnings.size + winMe(pool, winnings, depth + 1)

  def part2(input: String) =
    val cards = parse(input).toSeq
    winMe(cards, cards) + cards.length
  