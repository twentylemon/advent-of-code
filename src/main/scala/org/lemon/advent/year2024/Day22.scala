package org.lemon.advent.year2024

import org.lemon.advent.lib._

import scala.collection.parallel.CollectionConverters._

private object Day22:

  def parse(input: String) = input.linesIterator.map(_.toLong).toSeq

  extension (x: Long)
    def mix(y: Long) = x ^ y
    def prune = x & 0x00ffffff

  def process(secret: Long): Long =
    val one = (secret * 64).mix(secret).prune
    val two = (one / 32).mix(one).prune
    (two * 2048).mix(two).prune

  def part1(input: String) =
    parse(input)
      .map(seed => Iterator.iterate(seed)(process).nth(2000))
      .sum

  def price(secret: Long) = secret +% 10L

  def sequences(secret: Long) =
    Iterator.iterate(secret)(process)
      .sliding(2)
      .map { case Seq(a, b) => (price(b), price(b) - price(a)) }
      .take(2000)
      .sliding(4)
      .map { case Seq((_, a), (_, b), (_, c), (price, d)) => (a, b, c, d) -> price }
      .distinctBy(_._1)
      .toMap

  def part2(input: String) =
    val stores = parse(input).par
      .map(sequences)
      .toSeq
    val best = stores.flatMap(_.keys).toSet.maxBy(seq => stores.map(_.getOrElse(seq, 0L)).sum)
    stores.map(_.getOrElse(best, 0L)).sum
