package org.lemon.advent.year2023

import scala.collection.mutable

private object Day12:

  def parse(input: String) = input.linesIterator
    .map(_.split(" "))
    .map { case Array(springs, counts) => (springs, counts.split(",").map(_.toInt).toSeq) }

  def countPossible(springs: String, notes: Seq[Int]) =
    def loop(springs: String, notes: Seq[Int], chunkSize: Int)(using
        memory: mutable.Map[(String, Seq[Int], Int), Long]
    ): Long =
      val result: Long = memory.get((springs, notes, chunkSize)) match
        case Some(memorized) => memorized
        case None =>
          if springs.isEmpty then
            if notes.isEmpty && chunkSize == 0 then 1
            else if notes.headOption.contains(chunkSize) && notes.size == 1 then 1
            else 0
          else
            springs.head match
              case '#' =>
                if notes.headOption.contains(chunkSize) then 0
                else loop(springs.tail, notes, chunkSize + 1)
              case '.' =>
                if notes.headOption.contains(chunkSize) then loop(springs.tail, notes.tail, 0)
                else if chunkSize == 0 then loop(springs.tail, notes, 0)
                else 0
              case '?' => loop('#' + springs.tail, notes, chunkSize) + loop('.' + springs.tail, notes, chunkSize)
      memory += ((springs, notes, chunkSize) -> result)
      result

    given mutable.Map[(String, Seq[Int], Int), Long] = mutable.Map.empty[(String, Seq[Int], Int), Long]
    loop(springs, notes, 0)

  def part1(input: String) = parse(input)
    .map(countPossible)
    .sum

  def part2(input: String) = parse(input)
    .map((springs, notes) => (Seq.fill(5)(springs).mkString("?"), Seq.fill(5)(notes).flatten))
    .map(countPossible)
    .sum
