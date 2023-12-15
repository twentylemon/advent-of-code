package org.lemon.advent.year2023

private object Day15:

  def parse(input: String) = input

  def hash(part: String) = part.map(_.toInt).foldLeft(0)((code, ch) => (code + ch) * 17 % 256)

  def part1(input: String) = input.split(",")
    .map(_.trim)
    .map(hash)
    .sum

  def part2(input: String) = input.split(",")
    .map(_.trim)
    .map(chunk => if chunk.endsWith("-") then (chunk.dropRight(1), -1) else (chunk.dropRight(2), chunk.last.asDigit))
    // .map((chunk, digit) => (hash(chunk), digit))
    // .foldLeft(Map.empty[Int, Seq[(String, Int)]]) {
    //   case (map, (label, -1)) => map.updatedWith(hash(label))(seq => seq.map(_ :+ (label, -1)).orElse(Some(Seq((label, -1)))))
    //   // case (map, (label, digit))  if hash(label) >= seq.size => seq :+ (label, digit)
    //   // case (map, (label, digit)) => seq.updated(hash(label), (label, digit))
    // }
