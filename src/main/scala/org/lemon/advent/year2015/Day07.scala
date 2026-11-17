package org.lemon.advent.year2015

import org.lemon.advent.lib.*

private object Day07:

  type Expr = Expression[Int]
  type UnOp = UnaryOperation[Int]
  type BiOp = BinaryOperation[Int]

  case class And(lhs: Expr, rhs: Expr) extends BiOp:
    override def apply(left: Int, right: Int) = left & right & 0xffff

  case class Or(lhs: Expr, rhs: Expr) extends BiOp:
    override def apply(left: Int, right: Int) = (left | right) & 0xffff

  case class LeftShift(lhs: Expr, rhs: Expr) extends BiOp:
    override def apply(left: Int, right: Int) = (left << right) & 0xffff

  case class RightShift(lhs: Expr, rhs: Expr) extends BiOp:
    override def apply(left: Int, right: Int) = (left >>> right) & 0xffff

  case class Not(operand: Expr) extends UnOp:
    override def apply(operand: Int) = (~operand) & 0xffff

  def parse(input: String): Map[String, Expr] =
    def litOrRef(name: String) = name.toIntOption.map[Expr](Literal(_)).getOrElse(Reference[Int](name))
    input.linesIterator.map {
      case s"NOT $lhs -> $d" => d -> Not(Reference(lhs))
      case s"$lhs AND $rhs -> $d" => d -> And(litOrRef(lhs), litOrRef(rhs))
      case s"$lhs OR $rhs -> $d" => d -> Or(litOrRef(lhs), litOrRef(rhs))
      case s"$lhs LSHIFT $rhs -> $d" => d -> LeftShift(litOrRef(lhs), litOrRef(rhs))
      case s"$lhs RSHIFT $rhs -> $d" => d -> RightShift(litOrRef(lhs), litOrRef(rhs))
      case s"$lit -> $d" => d -> litOrRef(lit)
    }.toMap

  def part1(input: String, output: String = "a") =
    given ExpressionContext[Int] = ExpressionContext(parse(input))
    Reference(output).resolve.get

  def part2(input: String) =
    val expressions = parse(input)
    val a = Reference("a").resolve(using ExpressionContext(expressions)).get
    Reference("a").resolve(using ExpressionContext(expressions + ("b" -> Literal(a)))).get
