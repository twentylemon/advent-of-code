package org.lemon.advent.year2022

import org.lemon.UnitTest
import scala.util.Using
import scala.io.Source

class Day06Test extends UnitTest {

    private def firstDistinctGroup(in: String, window: Int) = in
        .sliding(window)
        .zipWithIndex
        .find((s, _) => s.distinct.length == window)
        .get._2 + window

    test("part 1 example") {
        firstDistinctGroup("mjqjpqmgbljsphdztnvjfqwrcgsmlb", 4) shouldBe 7
        firstDistinctGroup("bvwbjplbgvbhsrlpgdmjqwftvncz", 4) shouldBe 5
        firstDistinctGroup("nppdvjthqldpwncqszvftbrmjlhg", 4) shouldBe 6
        firstDistinctGroup("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 4) shouldBe 10
        firstDistinctGroup("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 4) shouldBe 11
    }

    test("part 1") {
        Using.resource(Source.fromResource("year2022/day06.txt"))(source =>
            firstDistinctGroup(source.mkString, 4) shouldBe 1892
        )
    }

    test("part 2 example") {
        firstDistinctGroup("mjqjpqmgbljsphdztnvjfqwrcgsmlb", 14) shouldBe 19
        firstDistinctGroup("bvwbjplbgvbhsrlpgdmjqwftvncz", 14) shouldBe 23
        firstDistinctGroup("nppdvjthqldpwncqszvftbrmjlhg", 14) shouldBe 23
        firstDistinctGroup("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 14) shouldBe 29
        firstDistinctGroup("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 14) shouldBe 26
    }

    test("part 2") {
        Using.resource(Source.fromResource("year2022/day06.txt"))(source =>
            firstDistinctGroup(source.mkString, 14) shouldBe 2313
        )
    }
}
