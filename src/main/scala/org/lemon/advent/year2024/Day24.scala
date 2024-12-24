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

  def calc(expressions: Map[String, Expression])(wire: String) =
    given ExpressionContext = ExpressionContext(expressions)
    expressions
      .filterKeys(_.startsWith(wire))
      .toSeq
      .sortBy(_._1)
      .zipWithIndex
      .map { case ((_, e), i) => (1L << i) * (if e.resolve then 1 else 0) }
      .sum

  def part1(input: String) =
    val expressions = parse(input)
    calc(expressions)("z")

  def checkAdditionBit(expressions: Map[String, Expression], suffixes: Iterable[String]) =
    // add bit is x_i ^ y_i ^ carry_{i-1}, so output must be XOR
    expressions
      .filterKeys(gate => gate.startsWith("z") && suffixes.exists(gate.endsWith))
      .filterNot(_._2 match
        case Op(_, _, "XOR") => true
        case _ => false
      )
      .toMap

  def checkCarryBit(expressions: Map[String, Expression], suffixes: Iterable[String]) =
    // carry bit is (a & b | a ^ b) & carry_{i-1}, so carry must be AND or OR
    expressions
      .filterKeys(!_.startsWith("z"))
      .filter(_._2 match
        case Op(Ref(r), _, _) if r.startsWith("x") || r.startsWith("y") => false
        case Op(_, Ref(r), _) if r.startsWith("x") || r.startsWith("y") => false
        case Op(_, _, "XOR") => true
        case _ => false
      )
      .toMap

  def lowestBitThatUses(gate: String, expressions: Map[String, Expression]): String =
    val uses = expressions
      .filter(_._2 match
        case Op(Ref(`gate`), _, _) | Op(_, Ref(`gate`), _) => true
        case _ => false
      )
    uses
      .find(_._1.startsWith("z"))
      .map((gate, _) => "z" + (gate.drop(1).toInt - 1).toString.padLeft(2, '0'))
      .getOrElse(uses.map(_._1).map(lowestBitThatUses(_, expressions)).min)

  extension (expressions: Map[String, Expression])
    def swap(gate1: String, gate2: String): Map[String, Expression] =
      expressions.updated(gate1, expressions(gate2)).updated(gate2, expressions(gate1))

  def part2(input: String) =
    val expressions = parse(input)

    def display(e: Map[String, Expression]) =
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

    val swapped = wrongCarries.foldLeft(expressions) { case (e, (gate, _)) =>
      e.swap(gate, lowestBitThatUses(gate, e))
    }

    // display(swapped)

    // at this point the adder is correct part for one bit
    val (x, y, z) = (calc(swapped)("x"), calc(swapped)("y"), calc(swapped)("z"))
    val diff = z ^ (x + y)
    val wrongBit = (diff.toBinaryString.size - 1).toString.padLeft(2, '0')
    // found by :eyes: at input... not sure how else this would be done
    val gongShow = swapped
      .filter(_._2 match
        case Op(Ref(x), Ref(y), _) if x.endsWith(wrongBit) && y.endsWith(wrongBit) => true
        case _ => false
      )

    // display(swapped.swap(gongShow.head._1, gongShow.last._1))
    (wrongAdds.keys ++ wrongCarries.keys ++ gongShow.keys).toSeq.sorted.mkString(",")
