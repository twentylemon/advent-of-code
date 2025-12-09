package org.lemon.advent.year2022

import org.lemon.advent.*
import scala.collection.mutable

class Day20Test extends UnitTest {

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

  def part1(in: Seq[String]) =
    val m = mix(in.map(_.toInt))
    val i = m.indexWhere(_ == 0)
    m((i + 1000) % m.size) + m((i + 2000) % m.size) + m((i + 3000) % m.size)

  def part2(in: Seq[String]) =
    val m = mix(in.map(_.toInt).map(811589153L * _), 10)
    val i = m.indexWhere(_ == 0)
    m((i + 1000) % m.size) + m((i + 2000) % m.size) + m((i + 3000) % m.size)

  test("mix example") {
    val in = """|1
                |2
                |-3
                |3
                |-2
                |0
                |4""".stripMargin
    mix(in.linesIterator.toSeq.map(_.toInt)) shouldBe Seq(1, 2, -3, 4, 0, 3, -2)
  }

  test("part 1 example") {
    val in = """|1
                |2
                |-3
                |3
                |-2
                |0
                |4""".stripMargin
    part1(in.linesIterator.toSeq) shouldBe 4 + -3 + 2
  }

  test("part 1") {
    part1(readLines(file(2022)(20))) shouldBe 13183
  }

  test("part 2 example") {
    val in = """|1
                |2
                |-3
                |3
                |-2
                |0
                |4""".stripMargin
    part2(in.linesIterator.toSeq) shouldBe 811589153L + 2434767459L + -1623178306L
  }

  test("part 2") {
    part2(readLines(file(2022)(20))) shouldBe 6676132372578L
  }

}
