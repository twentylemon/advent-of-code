package org.lemon.advent.lib.expressions

import org.lemon.advent.*
import org.scalacheck.*
import org.scalacheck.Arbitrary.*
import org.scalacheck.commands.Commands

import scala.util.{Failure, Success, Try}

class ExpressionsTest extends UnitTest:
  def references(expression: Expression[Long]): Set[String] = expression match
    case Reference(name) => Set(name)
    case unary: UnaryOperation[Long] => references(unary.operand)
    case binary: BinaryOperation[Long] => references(binary.lhs) ++ references(binary.rhs)
    case _ => Set.empty

  case class UnaryOp[T](name: String, construct: Expression[T] => Expression[T], eval: T => T):
    override def toString = name

  case class BinaryOp[T](
      name: String,
      construct: (Expression[T], Expression[T]) => Expression[T],
      eval: (T, T) => T,
      allowsRhs: T => Boolean = (_: T) => true,
  ):
    override def toString = name

  class AcyclicCommands[T: Arbitrary](unaryOps: Seq[UnaryOp[T]], binaryOps: Seq[BinaryOp[T]]) extends Commands:
    case class Expected(value: Option[T], simplified: Expression[T])
    type State = Map[String, Expected]
    final class Sut(var context: ExpressionContext[T])

    override def canCreateNewSut(newState: State, initSuts: Iterable[State], runningSuts: Iterable[Sut]) = true
    override def newSut(state: State) = Sut(ExpressionContext(Map.empty))
    override def destroySut(sut: Sut) = ()
    override def initialPreCondition(state: State) = state.isEmpty
    override def genInitialState = Gen.const(Map.empty)
    override def genCommand(state: State) =
      val name = s"x${state.size}"
      val literal = arbitrary[T].map(DefineLiteral(name, _))
      val missing = arbitrary[String].suchThat(!_.startsWith("x")).map(DefineMissingReference(name, _))
      if state.isEmpty then Gen.frequency(3 -> literal, 1 -> missing)
      else
        val defined = Gen.oneOf(state.keys.toSeq)
        Gen.frequency(
          3 -> literal,
          1 -> missing,
          2 -> defined.map(DefineReference(name, _)),
          2 -> Gen.zip(Gen.oneOf(unaryOps), defined).map(DefineUnary(name, _, _)),
          4 -> Gen.zip(Gen.oneOf(binaryOps), defined, defined).map(DefineBinary(name, _, _, _)),
          4 -> defined.map(Resolve(_)),
          2 -> defined.map(Apply(_)),
          4 -> defined.map(Simplify(_)),
        )

    sealed abstract class Define(defined: String, expression: Expression[T], references: String*) extends UnitCommand:
      def expected(state: State): Expected
      override def run(sut: Sut) = sut.context = ExpressionContext(sut.context.expressions + (defined -> expression))
      override def nextState(state: State) = state.updated(defined, expected(state))
      override def preCondition(state: State) = !state.contains(defined) && references.forall(state.contains)
      override def postCondition(state: State, success: Boolean): Prop = success

    case class DefineLiteral(name: String, value: T) extends Define(name, Literal(value)):
      override def expected(state: State) = Expected(Some(value), Literal(value))

    case class DefineMissingReference(name: String, to: String) extends Define(name, Reference(to)):
      override def expected(state: State) = Expected(None, Reference(to))

    case class DefineReference(name: String, to: String) extends Define(name, Reference(to), to):
      override def expected(state: State) = state(to)

    case class DefineUnary(name: String, op: UnaryOp[T], of: String)
        extends Define(name, op.construct(Reference(of)), of):
      override def expected(state: State) =
        val value = state(of).value.map(op.eval)
        Expected(value, value.fold(op.construct(state(of).simplified))(Literal(_)))

    case class DefineBinary(name: String, op: BinaryOp[T], lhs: String, rhs: String)
        extends Define(name, op.construct(Reference(lhs), Reference(rhs)), lhs, rhs):
      override def preCondition(state: State) = super.preCondition(state) && state(rhs).value.forall(op.allowsRhs)
      override def expected(state: State) =
        val value = for l <- state(lhs).value; r <- state(rhs).value yield op.eval(l, r)
        Expected(value, value.fold(op.construct(state(lhs).simplified, state(rhs).simplified))(Literal(_)))

    case class Resolve(name: String) extends SuccessCommand:
      type Result = Option[T]
      override def run(sut: Sut) = sut.context.resolve(name)
      override def nextState(state: State) = state
      override def preCondition(state: State) = state.contains(name)
      override def postCondition(state: State, result: Option[T]): Prop = result == state(name).value

    case class Apply(name: String) extends Command:
      type Result = T
      override def run(sut: Sut) = sut.context(name)
      override def nextState(state: State) = state
      override def preCondition(state: State) = state.contains(name)
      override def postCondition(state: State, result: Try[T]): Prop = (state(name).value, result) match
        case (Some(value), Success(applied)) => applied == value
        case (None, Failure(_: NoSuchElementException)) => true
        case _ => false

    case class Simplify(name: String) extends SuccessCommand:
      type Result = Expression[T]
      override def run(sut: Sut) = sut.context.simplify(name)
      override def nextState(state: State) = state
      override def preCondition(state: State) = state.contains(name)
      override def postCondition(state: State, result: Expression[T]): Prop = result == state(name).simplified

  test("resolve and simplify of numeric expressions match a model of the definitions") {
    val commands = AcyclicCommands[Long](
      Seq(UnaryOp("Negate", Negate(_), -_)),
      Seq(
        BinaryOp("Add", Add(_, _), _ + _),
        BinaryOp("Subtract", Subtract(_, _), _ - _),
        BinaryOp("Multiply", Multiply(_, _), _ * _),
        BinaryOp("Divide", Divide(_, _), _ / _, allowsRhs = _ != 0),
      ),
    )
    check(commands.property())
  }

  test("resolve and simplify of logic expressions match a model of the definitions") {
    val commands = AcyclicCommands[Boolean](
      Seq(UnaryOp("Not", Not(_), !_)),
      Seq(BinaryOp("And", And(_, _), _ && _), BinaryOp("Or", Or(_, _), _ || _), BinaryOp("Xor", Xor(_, _), _ ^ _)),
    )
    check(commands.property())
  }

  object CycleCommands extends Commands:
    case class State(definitions: Map[String, Expression[Long]], ringMember: Map[String, String])
    final class Sut(var context: ExpressionContext[Long])

    val link: Gen[String => Expression[Long]] = Gen.oneOf(
      Gen.const((next: String) => Reference[Long](next)),
      Gen.const((next: String) => Negate(Reference[Long](next))),
      arbitrary[Long].map(n => (next: String) => Add(Reference(next), Literal(n))),
      arbitrary[Long].map(n => (next: String) => Add(Literal(n), Reference(next))),
    )

    override def canCreateNewSut(newState: State, initSuts: Iterable[State], runningSuts: Iterable[Sut]) = true
    override def newSut(state: State) = Sut(ExpressionContext(Map.empty))
    override def destroySut(sut: Sut) = ()
    override def initialPreCondition(state: State) = state.definitions.isEmpty
    override def genInitialState = Gen.const(State(Map.empty, Map.empty))
    override def genCommand(state: State) =
      val size = state.definitions.size
      val ring = Gen.resize(4, Gen.nonEmptyListOf(link)).map(links =>
        def nameAt(i: Int) = s"x${size + i % links.size}"
        DefineRing(links.indices.map(i => nameAt(i) -> links(i)(nameAt(i + 1))).toMap)
      )
      if state.definitions.isEmpty then ring
      else
        val defined = Gen.choose(0, size - 1).map(i => s"x$i")
        val members = defined.map(state.ringMember)
        Gen.frequency(
          1 -> ring,
          2 -> Gen.zip(defined, link).map((on, link) => DefineDependent(s"x$size", link(on), on)),
          3 -> defined.map(Resolve(_)),
          3 -> defined.map(Simplify(_)),
          3 -> Gen.zip(members, arbitrary[Long]).map(SimplifyPinned(_, _)),
        )

    sealed abstract class Define(defined: Map[String, Expression[Long]], references: String*) extends UnitCommand:
      def ringMember(state: State): Map[String, String]
      override def run(sut: Sut) = sut.context = ExpressionContext(sut.context.expressions ++ defined)
      override def nextState(state: State) =
        State(state.definitions ++ defined, state.ringMember ++ ringMember(state))
      override def preCondition(state: State) =
        defined.keys.forall(!state.definitions.contains(_)) && references.forall(state.definitions.contains)
      override def postCondition(state: State, success: Boolean): Prop = success

    case class DefineRing(ring: Map[String, Expression[Long]]) extends Define(ring):
      override def ringMember(state: State) = ring.keys.map(name => name -> name).toMap

    case class DefineDependent(name: String, expression: Expression[Long], on: String)
        extends Define(Map(name -> expression), on):
      override def ringMember(state: State) = Map(name -> state.ringMember(on))

    sealed abstract class Query[R](name: String, f: ExpressionContext[Long] => R) extends SuccessCommand:
      type Result = R
      override def run(sut: Sut) = f(sut.context)
      override def nextState(state: State) = state
      override def preCondition(state: State) = state.definitions.contains(name)

    case class Resolve(name: String) extends Query(name, _.resolve(name)):
      override def postCondition(state: State, result: Option[Long]): Prop = result.isEmpty

    case class Simplify(name: String) extends Query(name, _.simplify(name)):
      override def postCondition(state: State, result: Expression[Long]): Prop =
        references(result) == Set(state.ringMember(name))

    case class SimplifyPinned(name: String, pin: Long) extends Query(name, _.simplify(name)):
      override def preCondition(state: State) = state.ringMember.get(name).contains(name)
      override def postCondition(state: State, result: Expression[Long]): Prop =
        def pinned = ExpressionContext(state.definitions.updated(name, Literal(pin)))
        result.resolve(pinned) == state.definitions(name).resolve(pinned)

  test("names that reach a reference cycle resolve to None and simplify to the ring member they reach") {
    check(CycleCommands.property())
  }
