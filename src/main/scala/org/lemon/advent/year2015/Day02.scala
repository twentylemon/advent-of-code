package org.lemon.advent.year2015

import org.lemon.advent.lib.geometry.*

private object Day02:

  def parse(input: String) = input.linesIterator.map {
    case s"${l}x${w}x${h}" => Cuboid(l.toInt, w.toInt, h.toInt)
  }.toSeq

  def part1(input: String) =
    val boxes = parse(input)
    boxes.map(box => box.surfaceArea + Seq(box.xy, box.xz, box.yz).map(_.size).min).sum

  def part2(input: String) =
    val boxes = parse(input)
    boxes.map(box => Seq(box.xy, box.xz, box.yz).map(_.perimeter).min + box.size).sum
