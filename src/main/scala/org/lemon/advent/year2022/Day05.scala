package org.lemon.advent.year2022

import org.lemon.advent.lib.*

private object Day05:

  private def parseStacks(lines: Seq[String]): Seq[Seq[Char]] = lines
    .map(_.toArray.toSeq)
    .transpose
    .collect(_.filter(_.isLetter))
    .filterNot(_.isEmpty)

  private def applyMove(reverse: Boolean)(stacks: Seq[Seq[Char]], line: String) = line match
    case s"move $n from $from to $to" =>
      stacks.zipWithIndex.map {
        case (s, i) if i == from.toInt - 1 => s.drop(n.toInt)
        case (s, i) if i == to.toInt - 1 =>
          val toMove = stacks(from.toInt - 1).take(n.toInt)
          if reverse then toMove.reverse :++ s else toMove :++ s
        case (s, _) => s
      }

  def part1(input: String) =
    val parts = input.chunks
    val stacks = parseStacks(parts(0).linesIterator.toSeq.dropRight(1))
    val moves = parts(1).linesIterator
    val result = moves.foldLeft(stacks)(applyMove(true))
    result.map(_.head).mkString

  def part2(input: String) =
    val parts = input.chunks
    val stacks = parseStacks(parts(0).linesIterator.toSeq.dropRight(1))
    val moves = parts(1).linesIterator
    val result = moves.foldLeft(stacks)(applyMove(false))
    result.map(_.head).mkString
