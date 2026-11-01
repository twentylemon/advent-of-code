package org.lemon.advent.year2016

import org.lemon.advent.lib.*

import java.security.MessageDigest
import scala.collection.parallel.CollectionConverters.*

private object Day14:

  private val instance = ThreadLocal.withInitial(() => MessageDigest.getInstance("MD5"))

  def md5(input: String): Int => Seq[Int] = memoize { idx =>
    instance.get()
      .digest(s"$input$idx".getBytes)
      .flatMap(b => Seq((b & 0xf0) >>> 4, b & 0x0f))
  }

  def triple(hash: Seq[Int]) = hash.sliding3.find((a, b, c) => a == b && b == c).map(_._1)

  def quintuples(hash: Seq[Int]) = hash.sliding(5).filter(w => w.forall(_ == w.head)).map(_.head).toSeq.distinct

  def run(md: Int => Seq[Int]) =
    Iterator.from(0)
      .grouped(500)
      .flatMap(_.par.flatMap(idx =>
        quintuples(md(idx)).flatMap(digit => ((idx - 1000).max(0) until idx).filter(i => triple(md(i)).contains(digit)))
      ).distinct.toVector.sorted)
      .nth(63)

  def part1(input: String) =
    run(md5(input.trim))

  def stretch(input: String): Int => Seq[Int] = memoize { idx =>
    val md = instance.get()
    (0 to 2016)
      .foldLeft(s"$input$idx")((str, _) => md.digest(str.getBytes).map("%02x".format(_)).mkString)
      .map(Character.digit(_, 16))
  }

  def part2(input: String) =
    run(stretch(input.trim))
