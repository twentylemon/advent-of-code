package org.lemon.advent.year2015

import org.lemon.advent.lib.*

private object Day15:

  case class Ingredient(capacity: Int, durability: Int, flavour: Int, texture: Int, calories: Int)

  def parse(input: String) = input.linesIterator.map(_ match
    case s"$_: capacity $c, durability $d, flavor $f, texture $t, calories $cal" =>
      Ingredient(capacity = c.toInt, durability = d.toInt, flavour = f.toInt, texture = t.toInt, calories = cal.toInt)
  ).toIndexedSeq

  def score(ingredients: Seq[Ingredient], counts: Seq[Int]) = ingredients.zip(counts)
    .map((i, n) => (n * i.capacity, n * i.durability, n * i.flavour, n * i.texture))
    .reduce { case ((a, b, c, d), (w, x, y, z)) => (a + w, b + x, c + y, d + z) }
    .toList
    .map(_ `max` 0)
    .product

  def enumerateSummation(total: Int, operands: Int): Iterator[Seq[Int]] =
    if operands <= 1 then Iterator(Seq(total))
    else
      for
        term <- (0 until total).iterator
        rest <- enumerateSummation(total - term, operands - 1)
      yield term +: rest

  def part1(input: String) =
    val ingredients = parse(input)
    enumerateSummation(100, ingredients.size)
      .map(score(ingredients, _))
      .max

  def calories(ingredients: Seq[Ingredient], counts: Seq[Int]) = ingredients.zip(counts)
    .map((i, c) => i.calories * c)
    .sum

  def part2(input: String) =
    val ingredients = parse(input)
    enumerateSummation(100, ingredients.size)
      .filter(calories(ingredients, _) == 500)
      .map(score(ingredients, _))
      .max
