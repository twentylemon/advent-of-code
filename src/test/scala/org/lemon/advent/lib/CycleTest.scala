package org.lemon.advent.lib

import org.lemon.advent.*
import org.scalacheck.Gen
import org.scalacheck.Prop.*

class CycleTest extends UnitTest:

  // a random function from 0 until size to itself, and a state to start from
  val functionalGraph =
    for
      size <- Gen.posNum[Int]
      next <- Gen.listOfN(size, Gen.choose(0, size - 1))
      initial <- Gen.choose(0, size - 1)
    yield (next.toVector, initial)

  test("findCycle and findCycleBy match a brute force search over random functional graphs") {
    check(forAll(functionalGraph) { (next: Vector[Int], initial: Int) =>
      // pigeonhole: some state must repeat within size + 1 steps, and size more steps run a period past it
      val states = Iterator.iterate(initial)(next).take(2 * next.size + 1).toVector
      val firstRepeat = states.indices.find(i => states.indexOf(states(i)) < i).get
      val offset = states.indexOf(states(firstRepeat))
      val expected = Cycle(offset, firstRepeat - offset, states.take(firstRepeat))
      Seq(findCycle(initial)(next), findCycleBy(initial)(next)).forall(cycle =>
        cycle == expected && states.indices.forall(n => cycle.stateAt(n) == states(n))
      )
    })
  }

  test("findCycle with eq finds the cycle in the keys") {
    check(forAll(functionalGraph) { (next: Vector[Int], initial: Int) =>
      val byKey = findCycle((initial, 0))((n, steps) => (next(n), steps + 1), _._1 == _._1)
      val plain = findCycle(initial)(next)
      byKey == plain.copy(history = plain.history.zipWithIndex)
    })
  }

  test("findCycleBy finds the cycle in the keys") {
    // the step count never repeats, so the cycle is only found through the key
    check(forAll(functionalGraph) { (next: Vector[Int], initial: Int) =>
      val byKey = findCycleBy((initial, 0))((n, steps) => (next(n), steps + 1), _._1)
      val plain = findCycleBy(initial)(next)
      byKey == plain.copy(history = plain.history.zipWithIndex)
    })
  }

  test("stateAt is periodic over the whole Long range") {
    val gen =
      for
        (next, initial) <- functionalGraph
        cycle = findCycle(initial)(next)
        n <- Gen.chooseNum(cycle.offset.toLong, Long.MaxValue)
        k <- Gen.chooseNum(0L, (Long.MaxValue - n) / cycle.period)
      yield (cycle, n, k)
    check(forAll(gen) { (cycle: Cycle[Int], n: Long, k: Long) =>
      cycle.stateAt(n + k * cycle.period) == cycle.stateAt(n)
    })
  }
