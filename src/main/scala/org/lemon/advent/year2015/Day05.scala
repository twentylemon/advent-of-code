package org.lemon.advent.year2015

import org.lemon.advent.lib.*

private object Day05:

  def nice(str: String) =
    def vowels = str.count(c => "aeiou".contains(c)) >= 3
    def double = str.sliding2.exists(_ == _)
    def naughty = str.contains("ab") || str.contains("cd") || str.contains("pq") || str.contains("xy")
    vowels && double && !naughty

  def part1(input: String) =
    input.linesIterator.count(nice)

  def supernice(str: String) =
    def pairpair = str.sliding(2).exists(s => str.indexOf(s, str.indexOf(s) + 2) != -1)
    def sandwich = str.sliding3.exists((a, _, c) => a == c)
    pairpair && sandwich

  def part2(input: String) =
    input.linesIterator.count(supernice)
