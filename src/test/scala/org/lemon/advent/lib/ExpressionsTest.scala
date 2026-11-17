package org.lemon.advent.lib

import org.lemon.advent.*
import org.scalacheck.*
import org.scalacheck.Arbitrary.*
import org.scalacheck.commands.Commands

class ExpressionsTest extends UnitTest:
  case class Add(lhs: Expression[Long], rhs: Expression[Long]) extends BinaryOperation[Long]:
    override def apply(left: Long, right: Long) = left + right

  case class Negate(operand: Expression[Long]) extends UnaryOperation[Long]:
    override def apply(operand: Long) = -operand

  def references(expression: Expression[Long]): Set[String] = expression match
    case Reference(name) => Set(name)
    case unary: UnaryOperation[Long] => references(unary.operand)
    case binary: BinaryOperation[Long] => references(binary.lhs) ++ references(binary.rhs)
    case _ => Set.empty

  object ExpressionCommands extends Commands:
    case class Expected(
        value: Option[Long],
        simplified: (String => Expression[Long]) => Expression[Long],
        inCycle: Boolean = false,
    )
    type State = Map[String, Expected]
    final class Sut(var context: ExpressionContext[Long])

    override def canCreateNewSut(newState: State, initSuts: Iterable[State], runningSuts: Iterable[Sut]) = true
    override def newSut(state: State) = Sut(ExpressionContext(Map.empty))
    override def destroySut(sut: Sut) = ()
    override def initialPreCondition(state: State) = state.isEmpty
    override def genInitialState = Gen.const(Map.empty)

    val link: Gen[String => Expression[Long]] = Gen.oneOf(
      Gen.const((next: String) => Reference[Long](next)),
      Gen.const((next: String) => Negate(Reference(next))),
      arbitrary[Long].map(n => (next: String) => Add(Reference(next), Literal(n))),
      arbitrary[Long].map(n => (next: String) => Add(Literal(n), Reference(next))),
    )

    override def genCommand(state: State) =
      val name = s"x${state.size}"
      val literal = arbitrary[Long].map(DefineLiteral(name, _))
      val unknown = Gen.const(DefineUnknown(name))
      val missing = arbitrary[String].suchThat(!_.startsWith("x")).map(DefineMissingReference(name, _))
      val cycle = Gen.nonEmptyListOf(link).map(links =>
        def nameAt(i: Int) = s"x${state.size + i % links.size}"
        DefineCycle(links.indices.map(i => nameAt(i) -> links(i)(nameAt(i + 1))).toMap)
      )
      if state.isEmpty then Gen.frequency(3 -> literal, 1 -> unknown, 1 -> missing, 1 -> cycle)
      else
        val defined = Gen.oneOf(state.keys.toSeq)
        Gen.frequency(
          3 -> literal,
          1 -> unknown,
          1 -> missing,
          1 -> cycle,
          2 -> defined.map(DefineReference(name, _)),
          2 -> defined.map(DefineNegate(name, _)),
          4 -> Gen.zip(defined, defined).map(DefineAdd(name, _, _)),
          4 -> defined.map(Resolve(_)),
          4 -> Gen.zip(defined, arbitrary[Long]).map(Simplify(_, _)),
        )

    abstract class Define(defined: String, expression: Expression[Long], references: String*) extends UnitCommand:
      def expected(state: State): Expected
      override def run(sut: Sut) = sut.context = ExpressionContext(sut.context.expressions + (defined -> expression))
      override def nextState(state: State) = state.updated(defined, expected(state))
      override def preCondition(state: State) = !state.contains(defined) && references.forall(state.contains)
      override def postCondition(state: State, success: Boolean): Prop = success

    case class DefineLiteral(name: String, n: Long) extends Define(name, Literal(n)):
      override def expected(state: State) = Expected(Some(n), _ => Literal(n))

    case class DefineUnknown(name: String) extends Define(name, Unknown()):
      override def expected(state: State) = Expected(None, _ => Unknown())

    case class DefineMissingReference(name: String, to: String) extends Define(name, Reference(to)):
      override def expected(state: State) = Expected(None, _ => Reference(to))

    case class DefineReference(name: String, to: String) extends Define(name, Reference(to), to):
      override def expected(state: State) = state(to).copy(inCycle = false)

    case class DefineNegate(name: String, of: String) extends Define(name, Negate(Reference(of)), of):
      override def expected(state: State) =
        val value = state(of).value.map(-_)
        Expected(value, simplifyCycle => value.fold(Negate(state(of).simplified(simplifyCycle)))(Literal(_)))

    case class DefineAdd(name: String, lhs: String, rhs: String)
        extends Define(name, Add(Reference(lhs), Reference(rhs)), lhs, rhs):
      override def expected(state: State) =
        val value = for l <- state(lhs).value; r <- state(rhs).value yield l + r
        Expected(
          value,
          simplifyCycle =>
            value.fold(Add(state(lhs).simplified(simplifyCycle), state(rhs).simplified(simplifyCycle)))(Literal(_)),
        )

    case class DefineCycle(cycle: Map[String, Expression[Long]]) extends UnitCommand:
      override def run(sut: Sut) = sut.context = ExpressionContext(sut.context.expressions ++ cycle)
      override def nextState(state: State) =
        state ++ cycle.keys.map(name => name -> Expected(None, simplifyCycle => simplifyCycle(name), inCycle = true))
      override def preCondition(state: State) = cycle.keys.forall(!state.contains(_))
      override def postCondition(state: State, success: Boolean): Prop = success

    case class Resolve(name: String) extends SuccessCommand:
      type Result = Option[Long]
      override def run(sut: Sut) = sut.context.resolve(name)
      override def nextState(state: State) = state
      override def preCondition(state: State) = state.contains(name)
      override def postCondition(state: State, result: Option[Long]): Prop = result == state(name).value

    case class Simplify(name: String, value: Long) extends SuccessCommand:
      type Result = (Expression[Long], ExpressionContext[Long])
      override def run(sut: Sut) = (sut.context.simplify(name), sut.context)
      override def nextState(state: State) = state
      override def preCondition(state: State) = state.contains(name)
      override def postCondition(state: State, result: Result): Prop =
        val (simplified, context) = result
        if state(name).inCycle then
          val withValue = ExpressionContext(context.expressions + (name -> Literal(value)))
          references(simplified) == Set(name)
          && simplified.resolve(withValue) == context.expressions(name).resolve(withValue)
        else simplified == state(name).simplified(context.simplify)

  test("resolve and simplify match a model of the definitions") {
    check(ExpressionCommands.property())
  }
