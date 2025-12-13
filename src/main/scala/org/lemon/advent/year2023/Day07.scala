package org.lemon.advent.year2023

import org.lemon.advent.lib.*

import scala.math.Ordering.Implicits.*

private object Day07:

  enum Rank:
    case Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace
  object Rank:
    def apply(char: Char): Rank =
      char match
        case 'A' => Ace
        case 'K' => King
        case 'Q' => Queen
        case 'J' => Jack
        case 'T' => Ten
        case d => Rank.fromOrdinal(d.toString.toInt - 2)

  enum Kind:
    case HighCard, Pair, TwoPair, ThreeOfKind, FullHouse, FourOfKind, FiveOfKind

  given Ordering[Kind] = Ordering.by(_.ordinal)

  case class Hand(cards: Seq[Rank], bid: Int):
    val freq: Map[Rank, Int] = cards.groupBy(x => x).mapValues(_.size).toMap

    val kind: Kind =
      freq.values.toSeq.sorted match
        case Seq(5) => Kind.FiveOfKind
        case Seq(1, 4) => Kind.FourOfKind
        case Seq(2, 3) => Kind.FullHouse
        case Seq(1, 1, 3) => Kind.ThreeOfKind
        case Seq(1, 2, 2) => Kind.TwoPair
        case Seq(1, 1, 1, 2) => Kind.Pair
        case _ => Kind.HighCard

    val jokerKind: Kind =
      val jokers = freq.getOrElse(Rank.Jack, 0)
      if jokers == 0 then kind
      else
        kind match
          case Kind.FiveOfKind | Kind.FourOfKind | Kind.FullHouse => Kind.FiveOfKind
          case Kind.ThreeOfKind => Kind.FourOfKind
          case Kind.Pair => Kind.ThreeOfKind
          case Kind.HighCard => Kind.Pair
          case Kind.TwoPair if jokers == 2 => Kind.FourOfKind
          case Kind.TwoPair if jokers != 2 => Kind.FullHouse
          case _ => throw AssertionError(kind)

  def parse(input: String) = input.linesIterator
    .map(_.wsv)
    .map { case Seq(hand, bid) => Hand(hand.map(Rank.apply), bid.toInt) }
    .toSeq

  def run(input: String)(using Ordering[Hand]) = parse(input)
    .sorted
    .map(_.bid)
    .zipWithIndex
    .map((bid, i) => bid * (i + 1))
    .sum

  def part1(input: String) =
    given Ordering[Rank] = Ordering.by(_.ordinal)
    given Ordering[Hand] = Ordering.by[Hand, Kind](_.kind)
      .orElse(Ordering.by[Hand, Seq[Rank]](_.cards))
    run(input)

  def part2(input: String) =
    given Ordering[Rank] with
      def compare(x: Rank, y: Rank): Int =
        if x == y then 0
        else if x == Rank.Jack then -1
        else if y == Rank.Jack then 1
        else x.ordinal.compareTo(y.ordinal)

    given Ordering[Hand] = Ordering.by[Hand, Kind](_.jokerKind)
      .orElse(Ordering.by[Hand, Seq[Rank]](_.cards))
    run(input)
