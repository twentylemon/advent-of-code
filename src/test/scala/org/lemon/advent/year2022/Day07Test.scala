package org.lemon.advent.year2022

import org.lemon.advent.*
import org.lemon.advent.year2022.Day07.*

class Day07Test extends UnitTest:

  test("part 1 example") {
    val in = """
            |$ cd /
            |$ ls
            |dir a
            |14848514 b.txt
            |8504156 c.dat
            |dir d
            |$ cd a
            |$ ls
            |dir e
            |29116 f
            |2557 g
            |62596 h.lst
            |$ cd e
            |$ ls
            |584 i
            |$ cd ..
            |$ cd ..
            |$ cd d
            |$ ls
            |4060174 j
            |8033020 d.log
            |5626152 d.ext
            |7214296 k""".stripMargin.strip

    buildTree(in.linesIterator).size shouldBe 48381165
    part1(in) shouldBe 95437
  }

  test("part 1") {
    part1(read(file(2022)(7))) shouldBe 1770595
  }

  test("part 2 example") {
    val in = """
            |$ cd /
            |$ ls
            |dir a
            |14848514 b.txt
            |8504156 c.dat
            |dir d
            |$ cd a
            |$ ls
            |dir e
            |29116 f
            |2557 g
            |62596 h.lst
            |$ cd e
            |$ ls
            |584 i
            |$ cd ..
            |$ cd ..
            |$ cd d
            |$ ls
            |4060174 j
            |8033020 d.log
            |5626152 d.ext
            |7214296 k""".stripMargin.strip

    part2(in) shouldBe 24933642
  }

  test("part 2") {
    part2(read(file(2022)(7))) shouldBe 2195372
  }
