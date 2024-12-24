package org.lemon.advent.year2024

import org.lemon.advent.lib._

private object Day24:

  case class ExpressionContext(expressions: Map[String, Expression])

  sealed trait Expression:
    def resolve(using context: ExpressionContext): Boolean

  case class Literal(bool: Boolean) extends Expression:
    override def resolve(using context: ExpressionContext) = bool

  case class Ref(name: String) extends Expression:
    override def resolve(using context: ExpressionContext) = context.expressions(name).resolve

  case class Op(lhs: Expression, rhs: Expression, op: String) extends Expression:
    override def resolve(using context: ExpressionContext) =
      op match
        case "AND" => lhs.resolve && rhs.resolve
        case "OR" => lhs.resolve || rhs.resolve
        case "XOR" => lhs.resolve ^ rhs.resolve

  def parse(input: String) =
    import org.lemon.advent.lib.parse.{given, _}
    input match
      case Chunk(init, gates) =>
        val lits = init.linesIterator.map(_ match
          case s"$k: $v" => k -> Literal(v.toInt >= 1)
        ).toMap
        val ops = gates.linesIterator.map(_ match
          case s"$lhs $op $rhs -> $out" => out -> Op(Ref(lhs), Ref(rhs), op)
        ).toMap
        lits ++ ops

  def part1(input: String) =
    val expressions = parse(input)
    calc(expressions)("z")

  def calc(expressions: Map[String, Expression])(wire: String) =
    given ExpressionContext = ExpressionContext(expressions)
    expressions
      .filterKeys(_.startsWith(wire))
      .toSeq
      .sortBy(_._1)
      .zipWithIndex
      .map { case ((_, e), i) => (1L << i) * (if e.resolve then 1 else 0) }
      .sum

  def checkSum(expressions: Map[String, Expression]) =
    calc(expressions)("x") + calc(expressions)("y") == calc(expressions)("z")

  def part2(input: String) =
    val expressions = parse(input)

    def display(e: Map[String, Expression]) =
      val x = calc(e)("x")
      val y = calc(e)("y")
      val z = calc(e)("z")
      println(s"x = ${x.toBinaryString.reverse.padTo(z.toBinaryString.size, '0').reverse}")
      println(s"y = ${y.toBinaryString.reverse.padTo(z.toBinaryString.size, '0').reverse}")
      println(s"z = ${z.toBinaryString}")
      println(s"d = ${(z - x - y).toBinaryString.reverse.padTo(z.toBinaryString.size, '0').reverse}")
      println()

    display(expressions)

    // display(expressions
    //   .updated("bwm", expressions("jhr"))
    //   .updated("jhr", expressions("bwm")))
    display(expressions
      .updated("nbb", expressions("vjc"))
      .updated("vjc", expressions("nbb")))
    0
