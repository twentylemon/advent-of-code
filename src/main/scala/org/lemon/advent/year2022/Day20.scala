package org.lemon.advent.year2022

import org.lemon.advent.lib.*
import scala.collection.mutable

private object Day20:

  def mix(seq: Seq[Long], times: Int = 1): Seq[Long] =
    val order = mutable.ArrayBuffer.from(seq.indices)

    for _ <- 1 to times; i <- seq.indices do
      val mixIndex = order.indexOf(i)
      order.remove(mixIndex)
      val posMod = (mixIndex + seq(i)) +% order.size.toLong
      order.insert(if posMod == 0 then order.size else posMod.toInt, i)

    order.map(seq).toSeq

  def part1(in: String) =
    val m = mix(in.linesIterator.map(_.toLong).toSeq)
    val i = m.indexWhere(_ == 0)
    m((i + 1000) % m.size) + m((i + 2000) % m.size) + m((i + 3000) % m.size)

  def part2(in: String) =
    val m = mix(in.linesIterator.map(_.toLong).map(811589153L * _).toSeq, 10)
    val i = m.indexWhere(_ == 0)
    m((i + 1000) % m.size) + m((i + 2000) % m.size) + m((i + 3000) % m.size)
