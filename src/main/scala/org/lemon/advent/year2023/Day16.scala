package org.lemon.advent.year2023

import org.lemon.advent.lib.`2d`.*
import org.lemon.advent.lib.`2d`.Coord.*
import org.lemon.advent.lib.graph.*

import scala.collection.parallel.CollectionConverters.*

private object Day16:

  // we know the position and the velocity. deal with it.
  case class Photon(position: Coord, velocity: Coord)

  def parse = Coord.gridToMap

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
    def adjacency(p: Photon) = interact(grid(p.position), p).filter(x => grid.contains(x.position))
    fill(adjacency, photon).map(_.position).size

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
