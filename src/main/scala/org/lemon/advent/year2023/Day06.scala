package org.lemon.advent.year2023

private object Day06:
  def parse(input: String) =
    val ins = input.linesIterator
      .map(_ match
        case s"$_: $nums" => nums.split(" ").map(_.trim).map(_.toLongOption).flatten.toSeq
      )
      .toSeq
    ins(0).zip(ins(1))

  def quadratic(time: Long, dist: Long) =
    val (a, b, c) = (-1, time, -(dist + 1)) // dist+1 because we need to beat them
    val t1 = (-b + math.sqrt(b * b - 4 * a * c)) / (2 * a)
    val t2 = (-b - math.sqrt(b * b - 4 * a * c)) / (2 * a)
    if t1.isNaN || t2.isNaN then 0
    else (math.floor(math.max(t1, t2)) - math.ceil(math.min(t1, t2)) + 1).toLong

  def part1(input: String) = parse(input)
    .map(quadratic)
    .product

  def part2(input: String) =
    val single = parse(input)
      .map((t, d) => (t.toString, d.toString))
      .reduce { case ((t1, d1), (t2, d2)) => (t1 + t2, d1 + d2) }
    quadratic(single._1.toLong, single._2.toLong)
