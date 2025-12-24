package org.lemon.advent.year2016

import java.security.MessageDigest
import scala.collection.parallel.CollectionConverters.*

private object Day05:

  def goochieMd5(input: String)(idx: Int) =
    val hash = MessageDigest.getInstance("MD5").digest(s"$input$idx".getBytes)
    Option.when(hash(0) == 0 && hash(1) == 0 && (hash(2) & 0xf0) == 0)((idx, hash(2), ((hash(3) >> 4) & 0x0f).toByte))

  def part1(input: String) =
    val md = goochieMd5(input.trim)
    Iterator.from(1)
      .grouped(100_000)
      .flatMap(_.par.flatMap(md).toVector.sortBy(_._1))
      .map(tup => "%x".format(tup._2))
      .take(8)
      .mkString

  def part2(input: String) =
    val md = goochieMd5(input.trim)
    Iterator.from(1)
      .grouped(100_000)
      .flatMap(_.par.flatMap(md).toVector.sortBy(_._1))
      .filter((_, pos, _) => pos < 8)
      .distinctBy(_._2)
      .take(8)
      .toSeq
      .sortBy(_._2)
      .map((_, _, ch) => "%x".format(ch))
      .mkString
