package org.lemon.advent.year2024

import org.lemon.advent.*
import org.lemon.advent.year2024.Day15.*

class Day15Test extends UnitTest:

  test("part 1 small example") {
    val in = """|########
                |#..O.O.#
                |##@.O..#
                |#...O..#
                |#.#.O..#
                |#...O..#
                |#......#
                |########
                |
                |<^^>>>vv<v>>v<""".stripMargin
    part1(in) shouldBe 2028
  }

  test("part 1 large example") {
    val in = """|##########
                |#..O..O.O#
                |#......O.#
                |#.OO..O.O#
                |#..O@..O.#
                |#O#..O...#
                |#O..O..O.#
                |#.OO.O.OO#
                |#....O...#
                |##########
                |
                |<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
                |vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
                |><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
                |<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
                |^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
                |^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
                |>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
                |<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
                |^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
                |v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^""".stripMargin
    part1(in) shouldBe 10092
  }

  test("part 1") {
    part1(read(file(2024)(15))) shouldBe 1448589
  }

  test("part 2 small example") {
    val in = """|#######
                |#...#.#
                |#.....#
                |#..OO@#
                |#..O..#
                |#.....#
                |#######
                |
                |<vv<<^^<<^^""".stripMargin
    part2(in) shouldBe 618
  }

  test("part 2 large example") {
    val in = """|##########
                |#..O..O.O#
                |#......O.#
                |#.OO..O.O#
                |#..O@..O.#
                |#O#..O...#
                |#O..O..O.#
                |#.OO.O.OO#
                |#....O...#
                |##########
                |
                |<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
                |vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
                |><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
                |<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
                |^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
                |^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
                |>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
                |<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
                |^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
                |v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^""".stripMargin
    part2(in) shouldBe 9021
  }

  test("part 2") {
    part2(read(file(2024)(15))) shouldBe 1472235
  }
