package org.lemon.advent.year2023

import cats._
import cats.implicits._
import cats.collections._
import cats.collections.syntax.all._
import scala.collection.mutable

private object Day19:

  case class Gear(x: Int, m: Int, a: Int, s: Int)

  sealed trait Flow:
    def dest: String
  case class Switch(v: Char, op: Char, lit: Int, dest: String) extends Flow
  case class Otherwise(dest: String) extends Flow

  case object Approved
  case object Rejected

  def parseGears(input: String) = input.linesIterator
    .map(_ match
      case s"{x=$x,m=$m,a=$a,s=$s}" => Gear(x = x.toInt, m = m.toInt, a = a.toInt, s = s.toInt)
    )
    .toSeq

  def parseFlow(flow: String) = flow.split(",")
    .map(_ match
      case s"$v>$n:$dest" => Switch(v = v.head, op = '>', lit = n.toInt, dest = dest)
      case s"$v<$n:$dest" => Switch(v = v.head, op = '<', lit = n.toInt, dest = dest)
      case otherwise => Otherwise(dest = otherwise)
    )
    .toSeq

  def parseFlows(input: String) = input.linesIterator
    .map(_ match
      case s"$name{$flow}" => (name, parseFlow(flow))
    )
    .toMap

  def parse(input: String) =
    val Array(flows, gears) = input.split("\n\n")
    (parseFlows(flows), parseGears(gears))

  def isApproved(dest: String) = dest == "A"
  def isRejected(dest: String) = dest == "R"
  def isFinal(flow: Flow) = isApproved(flow.dest) || isRejected(flow.dest)

  val full: Diet[Int] = Diet.fromRange(Int.MinValue toIncl Int.MaxValue)
  case class Tree(x: Diet[Int], m: Diet[Int], a: Diet[Int], s: Diet[Int])

  def simplify(flows: Map[String, Seq[Flow]]) =
    given Ordering[Seq[Flow]] = Ordering.by[Seq[Flow], Int](_.size).orElseBy(s => -s.count(isFinal))
    val ordered = flows.toSeq.sortBy(_._2)

    ordered.foreach(println)

    val reduced = mutable.Map.empty[String, Tree]
    val evalQueue = mutable.Queue.from(flows.toSeq.sortBy(_._2))

    println()

    def reduce(name: String, flows: Seq[Flow]) =
      flows.foldLeft(Tree(full, full, full, full))((tree, flow) =>
        flow match
          case Otherwise("A") => tree

          case Switch(_, _, _, "A") => tree
        
          case Switch(v, op, lit, "R") => tree
      )

    while !evalQueue.isEmpty do
      val (name, flows) = evalQueue.dequeue
      println(s"checking  $name = $flows")
      if reduced.contains(name) || flows.forall(isFinal) then
        println(s"  evaluating  $name = $flows")
        
      else
        println(s"  deferring   $name = $flows")
    0

  def part1(input: String) =
    val (flows, gears) = parse(input)

    flows.foreach(println)
    gears.foreach(println)

    0

  def part2(input: String) = 0
