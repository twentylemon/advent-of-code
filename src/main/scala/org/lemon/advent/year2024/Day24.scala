package org.lemon.advent.year2024

import org.lemon.advent.lib.*

private object Day24:

  type Expr = Expression[Boolean]
  type BiOp = BinaryOperation[Boolean]

  case class And(lhs: Expr, rhs: Expr) extends BiOp:
    override def apply(left: Boolean, right: Boolean) = left && right

  case class Or(lhs: Expr, rhs: Expr) extends BiOp:
    override def apply(left: Boolean, right: Boolean) = left || right

  case class Xor(lhs: Expr, rhs: Expr) extends BiOp:
    override def apply(left: Boolean, right: Boolean) = left ^ right

  def parse(input: String) =
    val Seq(init, gates) = input.chunks
    val lits = init.linesIterator.map {
      case s"$k: $v" => k -> Literal(v.toInt >= 1)
    }.toMap
    val ops = gates.linesIterator.map {
      case s"$lhs AND $rhs -> $out" => out -> And(Reference(lhs), Reference(rhs))
      case s"$lhs OR $rhs -> $out" => out -> Or(Reference(lhs), Reference(rhs))
      case s"$lhs XOR $rhs -> $out" => out -> Xor(Reference(lhs), Reference(rhs))
    }.toMap
    lits ++ ops

  def calc(expressions: Map[String, Expr])(wire: String) =
    val context = ExpressionContext(expressions)
    expressions
      .filterKeys(_.startsWith(wire))
      .toSeq
      .sortBy(_._1)
      .zipWithIndex
      .map { case ((_, e), i) => (1L << i) * (if e.resolve(context).get then 1 else 0) }
      .sum

  def part1(input: String) =
    val expressions = parse(input)
    calc(expressions)("z")

  def checkAdditionBit(expressions: Map[String, Expr], suffixes: Iterable[String]) =
    // add bit is x_i ^ y_i ^ carry_{i-1}, so output must be XOR
    expressions
      .filterKeys(gate => gate.startsWith("z") && suffixes.exists(gate.endsWith))
      .filterNot(_._2 match
        case _: Xor => true
        case _ => false)
      .toMap

  def checkCarryBit(expressions: Map[String, Expr], suffixes: Iterable[String]) =
    // carry bit is (a & b | a ^ b) & carry_{i-1}, so carry must be AND or OR
    expressions
      .filterKeys(!_.startsWith("z"))
      .filter(_._2 match
        case And(Reference(r), _) if r.startsWith("x") || r.startsWith("y") => false
        case And(_, Reference(r)) if r.startsWith("x") || r.startsWith("y") => false
        case Or(Reference(r), _) if r.startsWith("x") || r.startsWith("y") => false
        case Or(_, Reference(r)) if r.startsWith("x") || r.startsWith("y") => false
        case Xor(Reference(r), _) if r.startsWith("x") || r.startsWith("y") => false
        case Xor(_, Reference(r)) if r.startsWith("x") || r.startsWith("y") => false
        case _: Xor => true
        case _ => false)
      .toMap

  extension (expressions: Map[String, Expr])
    def swap(gate1: String, gate2: String): Map[String, Expr] =
      expressions.updated(gate1, expressions(gate2)).updated(gate2, expressions(gate1))

  def part2(input: String) =
    val expressions = parse(input)

    def display(e: Map[String, Expr]) =
      val (x, y, z) = (calc(e)("x"), calc(e)("y"), calc(e)("z"))
      println(s"x = ${x.toBinaryString.padLeft(z.toBinaryString.size, '0')}")
      println(s"y = ${y.toBinaryString.padLeft(z.toBinaryString.size, '0')}")
      println(s"z = ${z.toBinaryString}")
      println(s"+ = ${(x + y).toBinaryString.padLeft(z.toBinaryString.size, '0')}")
      println(s"^ = ${(z ^ (x + y)).toBinaryString.padLeft(z.toBinaryString.size, '0')}")
      println()

    // assumption is that the circuit is a regular full adder, there's no shenanigans in between
    val bitSuffixes = expressions.keys.filter(_.startsWith("x")).map(_.drop(1))
    val wrongAdds = checkAdditionBit(expressions, bitSuffixes)
    val wrongCarries = checkCarryBit(expressions, bitSuffixes)

    val swaps = for x <- wrongAdds.keys; y <- wrongCarries.keys yield (x, y)

    // swap adds and carries; there are 3 pairs out of 4
    swaps.triples.filter {
      case ((a, b), (c, d), (e, f)) => a != c && a != e && c != e && b != d && b != f && d != f
    }.flatMap {
      case ((a, b), (c, d), (e, f)) =>
        val swapped = expressions.swap(a, c).swap(b, d).swap(e, f)
        // display(swapped)
        val (x, y, z) = (calc(swapped)("x"), calc(swapped)("y"), calc(swapped)("z"))
        val diff = z ^ (x + y)
        val wrongBit = (diff.toBinaryString.size - 1).toString.padLeft(2, '0')
        // found by :eyes: at input... not sure how else this would be done
        val gongShow = swapped
          .filter(_._2 match
            case And(Reference(x), Reference(y)) if x.endsWith(wrongBit) && y.endsWith(wrongBit) => true
            case Or(Reference(x), Reference(y)) if x.endsWith(wrongBit) && y.endsWith(wrongBit) => true
            case Xor(Reference(x), Reference(y)) if x.endsWith(wrongBit) && y.endsWith(wrongBit) => true
            case _ => false)
        val last = swapped.swap(gongShow.head._1, gongShow.last._1)
        // display(last)
        Option.when(calc(last)("z") == calc(last)("x") + calc(last)("y"))(
          (Seq(a, b, c, d, e, f) ++ gongShow.keys).sorted.mkString(",")
        )
    }.next
