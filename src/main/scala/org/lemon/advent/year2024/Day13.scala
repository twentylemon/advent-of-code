package org.lemon.advent.year2024

private object Day13:

  type Coord = (Long, Long)

  def parse(input: String) = input.split("\n\n").map(_.linesIterator.toSeq match
    case Seq(
          s"Button A: X+$ax, Y+$ay",
          s"Button B: X+$bx, Y+$by",
          s"Prize: X=$px, Y=$py",
        ) => ((ax.toLong, ay.toLong), (bx.toLong, by.toLong), (px.toLong, py.toLong))
  ).toSeq

  def solve(a: Coord, b: Coord, prize: Coord) =
    val ((ax, ay), (bx, by), (px, py)) = (a, b, prize)
    // a * ax + b * bx = px  &  a * ay + b * by = py
    // => a = (px - b * bx) / ax  &  a = (py - b * by) / ay
    // => b = (px - a * ax) / bx  &  b = (py - a * ay) / by
    // => a = (by * px - bx * py) / (ax * by - ay * bx)  &  b = (ay * px - ax * py) / (ay * bx - ax by)
    val tryA = (by * px - bx * py) / (ax * by - ay * bx)
    val tryB = (px - tryA * ax) / bx

    if tryA * ax + tryB * bx == px && tryA * ay + tryB * by == py then (tryA, tryB)
    else (0L, 0L)

  def part1(input: String) =
    parse(input)
      .map(solve.tupled(_))
      .map(3 * _ + _)
      .sum

  def part2(input: String) =
    val shift = 10000000000000L
    parse(input)
      .map((a, b, prize) => (a, b, (prize._1 + shift, prize._2 + shift)))
      .map(solve.tupled(_))
      .map(3 * _ + _)
      .sum
