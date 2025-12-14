package org.lemon.advent.year2022

private object Day21:

  case class ExpressionContext(expressions: Map[String, Expression])

  sealed trait Expression:
    def resolve(using context: ExpressionContext): Option[Long]
    def simplify(using context: ExpressionContext): Expression

  case class Literal(n: Long) extends Expression:
    override def resolve(using context: ExpressionContext) = Some(n)
    override def simplify(using context: ExpressionContext) = this

  case class Reference(name: String) extends Expression:
    override def resolve(using context: ExpressionContext) = context.expressions(name).resolve
    override def simplify(using context: ExpressionContext) = context.expressions(name).simplify

  sealed trait BinaryOperation extends Expression:
    def lhs: Expression
    def rhs: Expression
    def op(x: Long, y: Long): Long
    def copy(lhs: Expression, rhs: Expression): BinaryOperation

    override def resolve(using context: ExpressionContext) = for x <- lhs.resolve; y <- rhs.resolve yield op(x, y)

    override def simplify(using context: ExpressionContext) = resolve match
      case Some(value) => Literal(value)
      case None => copy(lhs = lhs.simplify, rhs = rhs.simplify)

  case class Add(lhs: Expression, rhs: Expression) extends BinaryOperation:
    override def op(x: Long, y: Long) = x + y

  case class Subtract(lhs: Expression, rhs: Expression) extends BinaryOperation:
    override def op(x: Long, y: Long) = x - y

  case class Multiply(lhs: Expression, rhs: Expression) extends BinaryOperation:
    override def op(x: Long, y: Long) = x * y

  case class Divide(lhs: Expression, rhs: Expression) extends BinaryOperation:
    override def op(x: Long, y: Long) = x / y

  case object Unknown extends Expression:
    override def resolve(using context: ExpressionContext) = None
    override def simplify(using context: ExpressionContext) = this

  def parseExpression(line: String): (String, Expression) = line match
    case s"$v: $lhs $op $rhs" => (
        v,
        op match
          case "+" => Add(Reference(lhs), Reference(rhs))
          case "-" => Subtract(Reference(lhs), Reference(rhs))
          case "*" => Multiply(Reference(lhs), Reference(rhs))
          case "/" => Divide(Reference(lhs), Reference(rhs))
      )
    case s"$v: $lit" => (v, Literal(lit.toLong))

  def part1(in: String) =
    val monkeys = in.linesIterator.map(parseExpression).toMap
    given ExpressionContext = ExpressionContext(monkeys)
    monkeys("root").resolve.get

  def solve(simplifiedExpression: Expression, knownValue: Long): Long =
    simplifiedExpression match
      case Unknown => knownValue

      case Add(Literal(n), rhs) => solve(rhs, knownValue - n)
      case Add(lhs, Literal(n)) => solve(lhs, knownValue - n)

      case Subtract(Literal(n), rhs) => solve(rhs, n - knownValue)
      case Subtract(lhs, Literal(n)) => solve(lhs, n - -knownValue)

      case Multiply(Literal(n), rhs) => solve(rhs, knownValue / n)
      case Multiply(lhs, Literal(n)) => solve(lhs, knownValue / n)

      case Divide(Literal(n), rhs) => solve(rhs, n / knownValue)
      case Divide(lhs, Literal(n)) => solve(lhs, n * knownValue)
      case _ => ???

  def part2(in: String) =
    val monkeys = in.linesIterator.map(parseExpression).toMap
    given ExpressionContext = ExpressionContext(monkeys + ("humn" -> Unknown))

    val root = monkeys("root").asInstanceOf[BinaryOperation]
    val lhs = root.lhs.resolve
    val rhs = root.rhs.resolve

    lhs.map(solve(root.rhs.simplify, _)).orElse(rhs.map(solve(root.lhs.simplify, _))).get
