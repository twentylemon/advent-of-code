package org.lemon.advent.year2016

import org.lemon.advent.lib.`2d`.*
import org.lemon.advent.lib.graph.*

import scala.collection.mutable
import java.security.MessageDigest

private object Day17:

  private val md5 = MessageDigest.getInstance("MD5")

  def pathToString(path: Seq[Direction]) = path.map {
    case Direction.Up => 'U'
    case Direction.Down => 'D'
    case Direction.Left => 'L'
    case Direction.Right => 'R'
  }.mkString.reverse

  def adjacency(passcode: String)(node: (Coord, List[Direction])) =
    val (coord, path) = node
    val hash = md5.digest((passcode + pathToString(path)).getBytes)
    List(
      (Direction.Up, (hash(0) >> 4) & 0x0f),
      (Direction.Down, hash(0) & 0x0f),
      (Direction.Left, (hash(1) >> 4) & 0x0f),
      (Direction.Right, hash(1) & 0x0f)
    ).flatMap {
      case (dir, ch) if ch >= 0x0b && ch <= 0x0f =>
        val neigh = coord + dir
        if neigh.x >= 0 && neigh.x < 4 && neigh.y >= 0 && neigh.y < 4 then Some(neigh -> (dir :: path))
        else None
      case _ => None
    }

  def part1(input: String) =
    pathFind(adjacency(input.trim), (Coord(0, 0), List.empty[Direction]), _._1 == Coord(3, 3))
      .map(p => pathToString(p.path.last._2)).get

  def part2(input: String) =
    val pass = input.trim
    val paths = mutable.Set.empty[Int]
    val queue = mutable.Queue((Coord(0, 0), List.empty[Direction]))
    val end = Coord(3, 3)

    while queue.nonEmpty do
      val state @ (coord, path) = queue.dequeue()
      if coord == end then paths += path.length
      else queue ++= adjacency(pass)(state)

    paths.max
