package org.lemon.advent.year2023

import org.lemon.advent.lib.Coord2._
import org.lemon.advent.lib.Area
import scala.collection.parallel.CollectionConverters._
import scala.collection.mutable

private object Day16:

  // we know the position and the velocity. deal with it.
  case class Photon(position: Coord, velocity: Coord)

  def parse(input: String) = input.linesIterator
    .zipWithIndex
    .flatMap((line, row) => line.zipWithIndex.map((ch, col) => (Coord(col, row) -> ch)))
    .toMap

  def interact(cell: Char, photon: Photon) = cell match
    case '-' if photon.velocity.y != 0 =>
      Seq(Photon(photon.position.left, unitLeft), Photon(photon.position.right, unitRight))
    case '|' if photon.velocity.x != 0 =>
      Seq(Photon(photon.position.up, unitUp), Photon(photon.position.down, unitDown))
    case '/' =>
      val v = photon.velocity.flip * -1
      Seq(Photon(photon.position + v, v))
    case '\\' =>
      val v = photon.velocity.flip
      Seq(Photon(photon.position + v, v))
    case _ =>
      Seq(photon.copy(position = photon.position + photon.velocity))

  def countEnergized(grid: Map[Coord, Char], photon: Photon) =
    val seen = mutable.Set(photon)
    val queue = mutable.Queue(photon)

    while !queue.isEmpty do
      val light = queue.dequeue
      queue ++= interact(grid(light.position), light)
        .filter(p => grid.contains(p.position))
        .filter(seen.add)

    seen.map(_.position).size

  def part1(input: String) = countEnergized(parse(input), Photon(position = (0, 0), velocity = unitRight))

  def part2(input: String) =
    val grid = parse(input)
    val area = Area(grid)

    val photons = area.topRow.map(c => Photon(position = c, velocity = unitDown)) ++
      area.bottomRow.map(c => Photon(position = c, velocity = unitUp)) ++
      area.leftCol.map(c => Photon(position = c, velocity = unitRight)) ++
      area.rightCol.map(c => Photon(position = c, velocity = unitLeft))

    photons.toSeq.par
      .map(countEnergized(grid, _))
      .max
