package org.lemon.advent.year2015

import org.lemon.advent.lib.*

private object Day16:

  def parse(input: String) =
    input.linesIterator.map {
      case s"Sue $id: $props" => Map("id" -> id.toInt) ++ props.csv.map {
          case s"$key: $value" => key -> value.toInt
        }.toMap
    }.toSeq

  val target = Map(
    "children" -> 3,
    "cats" -> 7,
    "samoyeds" -> 2,
    "pomeranians" -> 3,
    "akitas" -> 0,
    "vizslas" -> 0,
    "goldfish" -> 5,
    "trees" -> 3,
    "cars" -> 2,
    "perfumes" -> 1
  )

  def part1(input: String) =
    val aunts = parse(input)
    aunts.find(_.removed("id").forall(target(_) == _)).get("id")

  def part2(input: String) =
    val aunts = parse(input)
    aunts.find(_.removed("id").forall((k, v) =>
      k match
        case "cats" | "trees" => v > target(k)
        case "pomeranians" | "goldfish" => v < target(k)
        case _ => v == target(k)
    )).get("id")
