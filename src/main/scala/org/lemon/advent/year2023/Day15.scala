package org.lemon.advent.year2023

import org.lemon.advent.lib.*

private object Day15:

  def hash(part: String) = part.map(_.toInt).foldLeft(0)((code, ch) => (code + ch) * 17 % 256)

  def part1(input: String) = input.csv
    .map(hash)
    .sum

  def part2(input: String) = input.csv
    .map(chunk => if chunk.endsWith("-") then (chunk.dropRight(1), -1) else (chunk.dropRight(2), chunk.last.asDigit))
    .map((label, op) => (label, hash(label), op))
    .foldLeft(Seq.fill(256)(Seq.empty[(String, Int)])) {
      case (boxes, (label, hsh, -1)) =>
        boxes.updated(hsh, boxes(hsh).filterNot(_._1 == label))
      case (boxes, (label, hsh, lens)) if boxes(hsh).exists(_._1 == label) =>
        boxes.updated(hsh, boxes(hsh).updated(boxes(hsh).indexWhere(_._1 == label), (label, lens)))
      case (boxes, (label, hsh, lens)) =>
        boxes.updated(hsh, boxes(hsh) :+ (label, lens))
    }
    .zipWithIndex
    .flatMap((seq, i) => seq.zipWithIndex.map { case ((_, lens), j) => (i + 1) * (j + 1) * lens })
    .sum
