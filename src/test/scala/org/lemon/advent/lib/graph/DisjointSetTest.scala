package org.lemon.advent.lib.graph

import org.lemon.advent.*
import org.scalacheck.*
import org.scalacheck.Prop.*
import org.scalacheck.commands.Commands

import scala.util.{Failure, Try}

class DisjointSetTest extends UnitTest:
  object DisjointSetCommands extends Commands:
    type State = Set[Set[Int]]
    final class Sut(var set: DisjointSet[Int])

    val element = Gen.choose(0, 40)
    def contains(state: State, a: Int) = state.exists(_(a))
    def missing[R](result: Try[R]) = result match
      case Failure(_: NoSuchElementException) => true
      case _ => false

    override def canCreateNewSut(newState: State, initSuts: Iterable[State], runningSuts: Iterable[Sut]) = true
    override def newSut(state: State) = Sut(DisjointSet(state.flatten))
    override def destroySut(sut: Sut) = ()
    override def initialPreCondition(state: State) = state.forall(_.size == 1)
    override def genInitialState = Gen.containerOf[Set, Int](element).map(_.map(Set(_)))
    override def genCommand(state: State) =
      Gen.oneOf(Gen.zip(element, element).map(Union(_, _)), Gen.zip(element, element).map(Connected(_, _)))

    case class Union(a: Int, b: Int) extends Command:
      type Result = Set[Set[Int]]
      override def run(sut: Sut) =
        sut.set = sut.set.union(a, b)
        sut.set.components.toSet
      override def nextState(state: State) =
        if !contains(state, a) || !contains(state, b) then state
        else
          val (groups, rest) = state.partition(group => group(a) || group(b))
          rest + groups.flatten
      override def preCondition(state: State) = true
      override def postCondition(state: State, result: Try[Result]): Prop =
        if contains(state, a) && contains(state, b) then result == Try(nextState(state)) else missing(result)

    case class Connected(a: Int, b: Int) extends Command:
      type Result = Boolean
      override def run(sut: Sut) = sut.set.connected(a, b)
      override def nextState(state: State) = state
      override def preCondition(state: State) = true
      override def postCondition(state: State, result: Try[Result]): Prop =
        if contains(state, a) && contains(state, b) then result == Try(state.exists(group => group(a) && group(b)))
        else missing(result)

  test("union and connected match a partition of the elements") {
    check(DisjointSetCommands.property())
  }
