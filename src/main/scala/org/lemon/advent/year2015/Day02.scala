package org.lemon.advent.year2015

import org.lemon.advent.lib.`2d`.*

private object Day02:

  def parse(input: String) = input.linesIterator.map(_ match
    case s"${l}x${w}x${h}" => (l.toInt, w.toInt, h.toInt)
  ).toSeq

  def part1(input: String) =
    val boxes = parse(input)
    boxes.map((l, w, h) =>
      val (lw, lh, wh) = (Area(l, w), Area(l, h), Area(w, h))
      2 * (lw.size + lh.size + wh.size) + (lw.size `min` lh.size `min` wh.size)
    ).sum

  def part2(input: String) =
    val boxes = parse(input)
    boxes.map((l, w, h) =>
      val (lw, lh, wh) = (Area(l, w), Area(l, h), Area(w, h))
      2 * (lw.boundary.size `min` lh.boundary.size `min` wh.boundary.size) + l * w * h
    ).sum
