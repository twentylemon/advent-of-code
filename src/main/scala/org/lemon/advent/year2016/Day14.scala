package org.lemon.advent.year2016

import org.lemon.advent.lib.*

import java.security.MessageDigest
import java.util.HexFormat
import scala.collection.parallel.CollectionConverters.*

private object Day14:

  private val instance = ThreadLocal.withInitial(() => MessageDigest.getInstance("MD5"))
  private val hex = HexFormat.of()

  def md5(input: String)(idx: Int) = hex.formatHex(instance.get().digest(s"$input$idx".getBytes))

  def triple(hash: String) = hash.sliding(3).find(w => w.forall(_ == w.head)).map(_.head)

  def quintuples(hash: String) = hash.sliding(5).filter(w => w.forall(_ == w.head)).map(_.head).toSeq.distinct

  def run(md: Int => String) =
    def tripQuin(idx: Int) =
      val hash = md(idx)
      (triple(hash), quintuples(hash))

    LazyList.from(0, 1000)
      .flatMap(start => (start until start + 1000).par.map(tripQuin).seq)
      .tails
      .zipWithIndex
      .collect { case (tqs, idx) if tqs.head._1.exists(t => tqs.tail.take(1000).exists(_._2.contains(t))) => idx }
      .nth(63)

  def part1(input: String) =
    run(md5(input.trim))

  def stretch(input: String)(idx: Int) =
    val md = instance.get()
    Iterator.iterate(s"$input$idx")(str => hex.formatHex(md.digest(str.getBytes))).nth(2016 + 1)

  def part2(input: String) =
    run(stretch(input.trim))
