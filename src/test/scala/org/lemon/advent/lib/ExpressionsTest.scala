package org.lemon.advent.lib

import org.lemon.advent.*
import org.scalacheck.*
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

    def canCreateNewSut(newState: State, initSuts: Iterable[State], runningSuts: Iterable[Sut]) = true
    def newSut(state: State) = Sut(ExpressionContext(Map.empty))
    def destroySut(sut: Sut) = ()
    def initialPreCondition(state: State) = state.isEmpty
    def genInitialState = Gen.const(Map.empty)
    def genCommand(state: State) =
      val name = s"x${state.size}"
      val literal = Arbitrary.arbitrary[Long].map(DefineLiteral(name, _))
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
      def run(sut: Sut) = sut.context = ExpressionContext(sut.context.expressions + (defined -> expression))
      def nextState(state: State) = state.updated(defined, expected(state))
      def preCondition(state: State) = !state.contains(defined) && references.forall(state.contains)
      def postCondition(state: State, success: Boolean): Prop = success

    case class DefineLiteral(name: String, n: Long) extends Define(name, Literal(n)):
      def expected(state: State) = Expected(Some(n), Literal(n))

    case class DefineUnknown(name: String) extends Define(name, Unknown()):
      def expected(state: State) = Expected(None, Unknown())

    case class DefineReference(name: String, to: String) extends Define(name, Reference(to), to):
      def expected(state: State) = state(to)

    case class DefineNegate(name: String, of: String) extends Define(name, Negate(Reference(of)), of):
      def expected(state: State) =
        val value = state(of).value.map(-_)
        Expected(value, value.fold[Expression[Long]](Negate(state(of).simplified))(Literal(_)))

    case class DefineAdd(name: String, lhs: String, rhs: String)
        extends Define(name, Add(Reference(lhs), Reference(rhs)), lhs, rhs):
      def expected(state: State) =
        val value = for l <- state(lhs).value; r <- state(rhs).value yield l + r
        Expected(value, value.fold[Expression[Long]](Add(state(lhs).simplified, state(rhs).simplified))(Literal(_)))

    case class Resolve(name: String) extends SuccessCommand:
      type Result = Option[Long]
      def run(sut: Sut) = sut.context.resolve(name)
      def nextState(state: State) = state
      def preCondition(state: State) = state.contains(name)
      def postCondition(state: State, result: Option[Long]): Prop = result == state(name).value

    case class Simplify(name: String) extends SuccessCommand:
      type Result = Expression[Long]
      def run(sut: Sut) = sut.context.simplify(name)
      def nextState(state: State) = state
      def preCondition(state: State) = state.contains(name)
      def postCondition(state: State, result: Expression[Long]): Prop = result == state(name).simplified

  test("resolve and simplify match a model of the definitions") {
    check(ExpressionCommands.property())
  }

  test("resolve of a reference cycle is None") {
    check(forAll(Gen.choose(1, 50), Gen.identifier) { (length, name) =>
      val cycle = (0 until length).map(i => s"$name$i" -> Reference[Long](s"$name${(i + 1) % length}")).toMap
      ExpressionContext(cycle).resolve(s"${name}0").isEmpty
    })
  }

  test("resolve of a missing name is None") {
    check((name: String, other: String, value: Long) =>
      name != other ==> ExpressionContext(Map(other -> Literal(value))).resolve(name).isEmpty
    )
  }
