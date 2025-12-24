package org.lemon.advent.year2016

import org.lemon.advent.lib.*

private object Day07:

  def cut(ip: String) = Iterator.unfold(ip)(ip =>
    Option.when(ip.nonEmpty) {
      val (supernet, rest1) = ip.span(_ != '[')
      val (hypernet, rest) = rest1.drop(1).span(_ != ']')
      (supernet, hypernet) -> rest.drop(1)
    }
  ).toSeq.unzip

  def abba(net: String) = net.sliding4.exists((a, b, c, d) => a == d && b == c && a != b)

  def tls(ip: String) =
    val (supernets, hypernets) = cut(ip)
    supernets.exists(abba) && !hypernets.exists(abba)

  def part1(input: String) =
    input.linesIterator.count(tls)

  def aba(net: String) = net.sliding3.filter((a, b, c) => a == c && a != b).map((a, b, _) => (a, b))

  def ssl(ip: String) =
    val (supernets, hypernets) = cut(ip)
    val babs = hypernets.flatMap(aba)
    supernets.flatMap(aba).exists((a, b) => babs.contains((b, a)))

  def part2(input: String) =
    input.linesIterator.count(ssl)
