package org.lemon.advent.year2015

import org.lemon.advent.lib.*

private object Day19:

  def parse(input: String) =
    val Seq(transitions, molecule) = input.chunks
    val replacements = transitions.linesIterator.map(_ match
      case s"$from => $to" => from -> to
    )
    (molecule, replacements.toSeq)

  def enumerate(molecule: String, replacements: Seq[(String, String)]) =
    for
      i <- molecule.indices
      (from, to) <- replacements
      if molecule.startsWith(from, i)
    yield molecule.take(i) + to + molecule.substring(i + from.size)

  def part1(input: String) =
    val (molecule, replacements) = parse(input)
    enumerate(molecule, replacements).toSet.size

  def part2(input: String) =
    val (molecule, _) = parse(input)
    val rn = molecule.sliding(2).count(_ == "Rn")
    val ar = molecule.sliding(2).count(_ == "Ar")
    val y = molecule.count(_ == 'Y')
    molecule.count(_.isUpper) - rn - ar - 2 * y - 1
