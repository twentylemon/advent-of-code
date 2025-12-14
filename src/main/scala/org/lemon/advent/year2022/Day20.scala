package org.lemon.advent.year2022

import scala.collection.mutable

private object Day20:

  extension (i: Long)
    def +%(n: Long): Long =
      val mod = i % n
      if mod < 0 then mod + n else mod

  def mix(seq: Seq[Long], times: Int = 1): Seq[Long] =
    val normalToMix = seq.indices.map(i => (i -> i)).to(mutable.Map)

    def shift(range: Range, amount: Int) =
      normalToMix
        .filter((_, mi) => range.contains(mi))
        .foreach((k, v) => normalToMix(k) += amount)

    for _ <- 1 to times; i <- seq.indices do
      val v = seq(i)
      val mixIndex = normalToMix(i)
      val posMod = (mixIndex + v) +% (seq.size - 1)
      val newMixIndex = if posMod == 0 then seq.size - 1 else posMod.toInt

      if mixIndex != newMixIndex then
        shift(newMixIndex until mixIndex by (mixIndex - newMixIndex).sign, (mixIndex - newMixIndex).sign)
      normalToMix(i) = newMixIndex

    seq.indices.sortBy(normalToMix).map(seq)

  def part1(in: String) =
    val m = mix(in.linesIterator.map(_.toLong).toSeq)
    val i = m.indexWhere(_ == 0)
    m((i + 1000) % m.size) + m((i + 2000) % m.size) + m((i + 3000) % m.size)

  def part2(in: String) =
    val m = mix(in.linesIterator.map(_.toLong).map(811589153L * _).toSeq, 10)
    val i = m.indexWhere(_ == 0)
    m((i + 1000) % m.size) + m((i + 2000) % m.size) + m((i + 3000) % m.size)
