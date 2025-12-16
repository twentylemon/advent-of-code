package org.lemon.advent.year2015

import java.security.MessageDigest
import scala.collection.parallel.CollectionConverters.*

private object Day04:

  def md5(input: String) = MessageDigest.getInstance("MD5").digest(input.getBytes).map("%02x".format(_)).mkString

  def part1(input: String) =
    val key = input.trim
    Iterator.from(1)
      .grouped(100_000)
      .flatMap(_.par.filter(i => md5(key + i).startsWith("00000")).seq.minOption)
      .next

  def part2(input: String) =
    val key = input.trim
    Iterator.from(1)
      .grouped(100_000)
      .flatMap(_.par.filter(i => md5(key + i).startsWith("000000")).seq.minOption)
      .next
