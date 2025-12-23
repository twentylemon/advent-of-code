package org.lemon.advent.year2016

import org.lemon.advent.lib.*

private object Day04:

  case class Room(name: String, sector: Int, checksum: String)

  def parse(input: String) = input.linesIterator.map(_ match
    case s"$prefix[$checksum]" =>
      val dashed = prefix.split('-')
      Room(name = dashed.init.mkString("-"), sector = dashed.last.toInt, checksum = checksum)
  ).toSeq

  def real(room: Room) =
    val top5 = room.name.replace("-", "").frequencies.toSeq.sortBy((ch, n) => (-n, ch)).take(5).map(_._1)
    top5.mkString == room.checksum

  def part1(input: String) =
    parse(input).filter(real).map(_.sector).sum

  def part2(input: String) =
    parse(input)
      .filter(real)
      .map(room =>
        val mapped = room.name.map(_ match
          case '-' => ' '
          case ch => ((ch - 'a' + room.sector) % 26 + 'a').toChar
        ).mkString
        room.copy(name = mapped)
      )
      .sortBy(_.name)
      .find(_.name == "northpole object storage")
      .map(_.sector)
      .get
