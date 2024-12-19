package org.lemon.advent

import scala.io.Source
import scala.util.Using
import sttp.client4._
import java.nio.file._
import java.time.LocalDate

def read(name: String): String =
  Using.resource(Source.fromResource(name))(source => source.mkString)

def readLines(name: String): Seq[String] =
  Using.resource(Source.fromResource(name))(source => source.getLines.toSeq)

def file(year: Int)(day: Int) = s"year$year/day${"%02d".format(day)}.txt"

@main
def setup(year: Int, day: Int) =
  assert(1 to 25 contains day, s"$day must be between 1 and 25")
  assert(!LocalDate.now.isBefore(LocalDate.of(year, 12, day)), s"$year/$day is not available yet")
  
  val path = file(year)(day)
  Using(Source.fromResource(path)) { _ =>
    println(s"$path already exists")
  }.getOrElse {
    Using.resource(Source.fromFile("session")) { source =>
      val javaPath = Paths.get(s"src/main/resources/$path")
      Files.createDirectories(javaPath.getParent)

      val response = quickRequest
        .get(uri"https://adventofcode.com/$year/day/$day/input")
        .cookie("session", source.mkString.trim)
        .send(DefaultSyncBackend())
        .body

      println(response)
      println(Files.write(javaPath, response.getBytes))
    }

    val src = Paths.get(s"src/main/scala/org/lemon/advent/year$year/Day${"%02d".format(day)}.scala")
    Files.createDirectories(src.getParent)
    Files.write(src, s"""package org.lemon.advent.year$year
                        |
                        |import org.lemon.advent.lib._
                        |
                        |private object Day${"%02d".format(day)}:
                        |
                        |  def parse(input: String) =
                        |    import org.lemon.advent.lib.parse.{given, _}
                        |    input
                        |
                        |  def part1(input: String) =
                        |    0
                        |
                        |  def part2(input: String) =
                        |    0
                        |""".stripMargin.getBytes)

    val test = Paths.get(s"src/test/scala/org/lemon/advent/year$year/Day${"%02d".format(day)}Test.scala")
    Files.createDirectories(test.getParent)
    Files.write(test, s"""package org.lemon.advent.year$year
                        |
                        |import org.lemon.advent._
                        |import org.lemon.advent.year$year.Day${"%02d".format(day)}._
                        |
                        |class Day${"%02d".format(day)}Test extends UnitTest:
                        |
                        |  test("part 1 example") {
                        |    val in = \"\"\"|
                        |                |\"\"\".stripMargin
                        |    part1(in) shouldBe 0
                        |  }
                        |
                        |  test("part 1") {
                        |    part1(read(file($year)($day))) shouldBe 0
                        |  }
                        |
                        |  test("part 2 example") {
                        |    val in = \"\"\"|
                        |                |\"\"\".stripMargin
                        |    part2(in) shouldBe 0
                        |  }
                        |
                        |  test("part 2") {
                        |    part2(read(file($year)($day))) shouldBe 0
                        |  }
                        |""".stripMargin.getBytes)
  }
