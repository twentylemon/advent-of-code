package org.lemon.advent.year2015

import java.security.MessageDigest
import scala.collection.parallel.CollectionConverters.*

private object Day04:

  def fiveZeros(input: String)(idx: Int) =
    val hash = MessageDigest.getInstance("MD5").digest(s"$input$idx".getBytes)
    Option.when(hash(0) == 0 && hash(1) == 0 && (hash(2) & 0xf0) == 0)(idx)

  def sixZeros(input: String)(idx: Int) =
    val hash = MessageDigest.getInstance("MD5").digest(s"$input$idx".getBytes)
    Option.when(hash(0) == 0 && hash(1) == 0 && hash(2) == 0)(idx)

  def part1(input: String) =
    val md = fiveZeros(input.trim)
    Iterator.from(1)
      .grouped(100_000)
      .flatMap(_.par.flatMap(md).seq.minOption)
      .next

  def part2(input: String) =
    val md = sixZeros(input.trim)
    Iterator.from(1)
      .grouped(100_000)
      .flatMap(_.par.flatMap(md).seq.minOption)
      .next
