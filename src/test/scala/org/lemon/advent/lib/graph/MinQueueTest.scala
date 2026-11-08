package org.lemon.advent.lib.graph

import org.lemon.advent.*
import org.scalacheck.*
import org.scalacheck.Prop.*
import org.scalacheck.commands.Commands

import scala.collection.immutable.TreeMap

class MinQueueTest extends UnitTest:
  object MinQueueCommands extends Commands:
    case class Bucket(elements: List[Int], remaining: Int)
    type State = Map[Int, Bucket]
    final class Sut(var queue: MinQueue[Int, Int])

    val priority = Gen.choose(0, 10)

    def canCreateNewSut(newState: State, initSuts: Iterable[State], runningSuts: Iterable[Sut]) = true
    def newSut(state: State) = Sut(MinQueue(TreeMap.empty[Int, List[Int]]))
    def destroySut(sut: Sut) = ()
    def initialPreCondition(state: State) = state.isEmpty
    def genInitialState = Gen.const(Map.empty)
    def genCommand(state: State) =
      Gen.oneOf(Gen.zip(priority, Arbitrary.arbitrary[Int]).map(Enqueue(_, _)), Gen.const(Dequeue))

    case class Enqueue(priority: Int, element: Int) extends UnitCommand:
      def run(sut: Sut) = sut.queue = sut.queue.enqueue(priority, element)
      def nextState(state: State) =
        val Bucket(elements, remaining) = state.getOrElse(priority, Bucket(Nil, 0))
        state.updated(priority, Bucket(element :: elements, remaining + 1))
      def preCondition(state: State) = true
      def postCondition(state: State, success: Boolean): Prop = success

    case object Dequeue extends SuccessCommand:
      type Result = Option[(Int, Int)]
      def run(sut: Sut) = sut.queue.dequeue.map((priority, element, rest) =>
        sut.queue = rest
        (priority, element)
      )
      def nextState(state: State) = state.minByOption(_._1) match
        case Some((priority, Bucket(_, 1))) => state - priority
        case Some((priority, bucket)) => state.updated(priority, bucket.copy(remaining = bucket.remaining - 1))
        case None => state
      def preCondition(state: State) = true
      def postCondition(state: State, result: Result): Prop = (state.minByOption(_._1), result) match
        case (None, None) => true
        case (Some((smallest, bucket)), Some((priority, element))) =>
          priority == smallest && bucket.elements.contains(element)
        case _ => false

  test("dequeue returns an element with the smallest priority still queued") {
    check(MinQueueCommands.property())
  }
