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
      val missing = arbitrary[String].suchThat(!_.startsWith("x")).map(DefineMissingReference(name, _))
      if state.isEmpty then Gen.frequency(3 -> literal, 1 -> unknown, 1 -> missing)
      else
        val defined = Gen.oneOf(state.keys.toSeq)
        Gen.frequency(
          3 -> literal,
          1 -> unknown,
          1 -> missing,
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

    case class DefineMissingReference(name: String, to: String) extends Define(name, Reference(to)):
      override def expected(state: State) = Expected(None, Reference(to))

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

  val linkGen: Gen[Expression[Long] => Expression[Long]] = Gen.oneOf(
    Gen.const((next: Expression[Long]) => next),
    Gen.const((next: Expression[Long]) => Negate(next)),
    arbitrary[Long].map(n => (next: Expression[Long]) => Add(next, Literal(n))),
    arbitrary[Long].map(n => (next: Expression[Long]) => Add(Literal(n), next)),
  )

  def cycleOf(prefix: String, links: Seq[Expression[Long] => Expression[Long]]) =
    links.indices.map(i => s"$prefix$i" -> links(i)(Reference(s"$prefix${(i + 1) % links.size}"))).toMap

  val cycleGen =
    for
      prefix <- Gen.identifier
      links <- Gen.nonEmptyListOf(linkGen)
    yield cycleOf(prefix, links)

  test("resolve of every name in a cycle is None") {
    check(forAll(cycleGen)(cycle => cycle.keys.map(ExpressionContext(cycle).resolve).forall(_.isEmpty)))
  }

  test("resolve of a name that depends on a cycle is None") {
    val gen =
      for
        cycle <- cycleGen
        target <- Gen.oneOf(cycle.keys)
        dependent <- arbitrary[String].suchThat(!cycle.contains(_))
      yield (cycle, target, dependent)
    check(forAll(gen) { (cycle, target, dependent) =>
      ExpressionContext(cycle + (dependent -> Reference(target))).resolve(dependent).isEmpty
    })
  }

  test("resolve of a missing name is None") {
    check((definitions: Map[String, Long], name: String) =>
      ExpressionContext((definitions - name).view.mapValues(Literal(_)).toMap).resolve(name).isEmpty
    )
  }

  test("simplify of a missing name is a reference to it") {
    check((definitions: Map[String, Long], name: String) =>
      ExpressionContext((definitions - name).view.mapValues(Literal(_)).toMap).simplify(name) == Reference(name)
    )
  }

  test("simplify of a name in a cycle applies every link once, ending at a reference to that name") {
    val gen = for prefix <- Gen.identifier; links <- Gen.nonEmptyListOf(linkGen) yield (prefix, links)
    check(forAll(gen) { (prefix, links) =>
      val context = ExpressionContext(cycleOf(prefix, links))
      links.indices.forall(i =>
        val unrolled = links.indices.foldRight[Expression[Long]](Reference(s"$prefix$i"))((k, inner) =>
          links((i + k) % links.size)(inner)
        )
        context.simplify(s"$prefix$i") == unrolled
      )
    })
  }
