package org.lemon.advent.year2022

import scala.collection.mutable

private object Day07:

  sealed trait File:
    def size: Int
    def name: String

  case class Directory(name: String, parent: Directory, contents: mutable.Set[File] = mutable.Set()) extends File:
    def size = contents.map(_.size).sum
  case class Obj(name: String, size: Int) extends File

  def buildTree(lines: Iterator[String]) =
    val root = Directory("/", parent = null)
    var pwd = root
    lines.foreach(_ match
      case "$ cd .." => pwd = pwd.parent
      case "$ cd /" => pwd = root
      case s"$$ cd $dir" =>
        pwd = pwd.contents.filter(_.isInstanceOf[Directory]).map(_.asInstanceOf[Directory]).find(_.name == dir).get
      case "$ ls" => "nothing to do"
      case s"dir $dir" => pwd.contents.add(Directory(dir, pwd))
      case s"$size $name" => pwd.contents.add(Obj(name = name, size = size.toInt))
    )
    root

  def iterator(tree: File): Iterator[File] = tree match
    case obj: Obj => Iterator(obj)
    case dir: Directory => Iterator(dir) ++ dir.contents.flatMap(iterator)

  def part1(input: String) =
    val root = buildTree(input.linesIterator)
    iterator(root)
      .filter(_.isInstanceOf[Directory])
      .map(_.size)
      .filter(_ <= 100000)
      .sum

  def part2(input: String) =
    val total = 70000000
    val required = 30000000
    val root = buildTree(input.linesIterator)
    val unused = total - root.size
    val needToIce = required - unused
    iterator(root)
      .filter(_.isInstanceOf[Directory])
      .map(_.size)
      .filter(_ >= needToIce)
      .min
