package org.lemon.advent.lib

import org.lemon.advent.*
import org.scalacheck.*
import org.scalacheck.Arbitrary.*
import org.scalacheck.Prop.*
import org.scalacheck.commands.Commands

class ExpressionsTest extends UnitTest:
  case class Add(lhs: Expression[Long], rhs: Expression[Long]) extends BinaryOperation[Long]:
    override def apply(left: Long, right: Long) = left + right

  case class Negate(operand: Expression[Long]) extends UnaryOperation[Long]:
    override def apply(operand: Long) = -operand

  object ExpressionCommands extends Commands:
    case class Expected(value: Option[Long], simplified: Expression[Long])
    type State = Map[String, Expected]
    final class Sut(var context: ExpressionContext[Long])

    override def canCreateNewSut(newState: State, initSuts: Iterable[State], runningSuts: Iterable[Sut]) = true
    override def newSut(state: State) = Sut(ExpressionContext(Map.empty))
    override def destroySut(sut: Sut) = ()
    override def initialPreCondition(state: State) = state.isEmpty
    override def genInitialState = Gen.const(Map.empty)
    override def genCommand(state: State) =
      val name = s"x${state.size}"
      val literal = arbitrary[Long].map(DefineLiteral(name, _))
      val unknown = Gen.const(DefineUnknown(name))
      if state.isEmpty then Gen.frequency(3 -> literal, 1 -> unknown)
      else
        val defined = Gen.oneOf(state.keys.toSeq)
        Gen.frequency(
          3 -> literal,
          1 -> unknown,
          2 -> defined.map(DefineReference(name, _)),
          2 -> defined.map(DefineNegate(name, _)),
          4 -> Gen.zip(defined, defined).map(DefineAdd(name, _, _)),
          4 -> defined.map(Resolve(_)),
          4 -> defined.map(Simplify(_)),
        )

    abstract class Define(defined: String, expression: Expression[Long], references: String*) extends UnitCommand:
      def expected(state: State): Expected
      override def run(sut: Sut) = sut.context = ExpressionContext(sut.context.expressions + (defined -> expression))
      override def nextState(state: State) = state.updated(defined, expected(state))
      override def preCondition(state: State) = !state.contains(defined) && references.forall(state.contains)
      override def postCondition(state: State, success: Boolean): Prop = success

    case class DefineLiteral(name: String, n: Long) extends Define(name, Literal(n)):
      override def expected(state: State) = Expected(Some(n), Literal(n))

    case class DefineUnknown(name: String) extends Define(name, Unknown()):
      override def expected(state: State) = Expected(None, Unknown())

    case class DefineReference(name: String, to: String) extends Define(name, Reference(to), to):
      override def expected(state: State) = state(to)

    case class DefineNegate(name: String, of: String) extends Define(name, Negate(Reference(of)), of):
      override def expected(state: State) =
        val value = state(of).value.map(-_)
        Expected(value, value.fold[Expression[Long]](Negate(state(of).simplified))(Literal(_)))

    case class DefineAdd(name: String, lhs: String, rhs: String)
        extends Define(name, Add(Reference(lhs), Reference(rhs)), lhs, rhs):
      override def expected(state: State) =
        val value = for l <- state(lhs).value; r <- state(rhs).value yield l + r
        Expected(value, value.fold[Expression[Long]](Add(state(lhs).simplified, state(rhs).simplified))(Literal(_)))

    case class Resolve(name: String) extends SuccessCommand:
      type Result = Option[Long]
      override def run(sut: Sut) = sut.context.resolve(name)
      override def nextState(state: State) = state
      override def preCondition(state: State) = state.contains(name)
      override def postCondition(state: State, result: Option[Long]): Prop = result == state(name).value

    case class Simplify(name: String) extends SuccessCommand:
      type Result = Expression[Long]
      override def run(sut: Sut) = sut.context.simplify(name)
      override def nextState(state: State) = state
      override def preCondition(state: State) = state.contains(name)
      override def postCondition(state: State, result: Expression[Long]): Prop = result == state(name).simplified

  test("resolve and simplify match a model of the definitions") {
    check(ExpressionCommands.property())
  }

  val cycleGen =
    val link: Gen[String => Expression[Long]] = Gen.oneOf(
      Gen.const((next: String) => Reference[Long](next)),
      Gen.const((next: String) => Negate(Reference(next))),
      arbitrary[Long].map(n => (next: String) => Add(Reference(next), Literal(n))),
      arbitrary[Long].map(n => (next: String) => Add(Literal(n), Reference(next))),
    )
    for
      prefix <- Gen.identifier
      links <- Gen.nonEmptyListOf(link)
    yield links.indices.map(i => s"$prefix$i" -> links(i)(s"$prefix${(i + 1) % links.size}")).toMap

  test("resolve of every name in a cycle is None") {
    check(forAll(cycleGen) { cycle =>
      cycle.keys.map(ExpressionContext(cycle).resolve).forall(_.isEmpty)
    })
  }

  test("resolve of a name that depends on a cycle is None, and names resolved along the way keep their values") {
    check(forAll(cycleGen, arbitrary[Long]) { (cycle, value) =>
      val context = ExpressionContext(
        cycle + ("dependent" -> Add(Reference("independent"), Reference(cycle.keys.head)))
          + ("independent" -> Literal(value))
      )
      context.resolve("dependent").isEmpty && context.resolve("independent") == Some(value)
    })
  }

  test("resolve of a missing name is None") {
    check((definitions: Map[String, Long], name: String) =>
      ExpressionContext((definitions - name).view.mapValues(Literal(_)).toMap).resolve(name).isEmpty
    )
  }
