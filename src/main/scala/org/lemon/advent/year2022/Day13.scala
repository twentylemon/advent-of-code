package org.lemon.advent.year2022

import io.circe.*
import io.circe.syntax.*
import io.circe.parser.*
import scala.math.Ordering.Implicits.*

private object Day13:

  def parseJson(line: String) = parse(line) match
    case Right(thing) => thing
    case Left(_) => throw AssertionError()

  extension (json: Json)
    def asInt = json.asNumber.get.toInt.get
    def asSeq: Seq[Json] = json.asArray.get

  given Ordering[Json] with
    def compare(lhs: Json, rhs: Json): Int =
      (lhs, rhs) match
        case (l, r) if l.isNumber && r.isNumber => l.asInt - r.asInt
        case (l, r) if l.isNumber && r.isArray => compare(Seq(l.asInt).asJson, r)
        case (l, r) if l.isArray && r.isNumber => compare(l, Seq(r.asInt).asJson)
        case (l, r) if l.isArray && r.isArray =>
          (l.asSeq, r.asSeq) match
            case (Seq(x, xs*), Seq(y, ys*)) =>
              val comp = compare(x, y)
              if comp != 0 then comp else compare(xs.asJson, ys.asJson)
            case (Seq(xs*), Seq(ys*)) => xs.lengthCompare(ys)
        case _ => throw AssertionError()

  def part1(input: String) = input.split("\n\n")
    .map(_.linesIterator.toSeq)
    .map(lines => (parseJson(lines(0)), parseJson(lines(1))))
    .zipWithIndex
    .filter({ case ((lhs, rhs), _) => lhs <= rhs })
    .map(_._2 + 1)
    .sum

  def part2(input: String) =
    val key1 = "[[2]]"
    val key2 = "[[6]]"
    val packets = input.split("\n\n").flatMap(_.linesIterator).appendedAll(Seq(key1, key2)).map(parseJson).sorted
    (packets.indexOf(parseJson(key1)) + 1) * (packets.indexOf(parseJson(key2)) + 1)
