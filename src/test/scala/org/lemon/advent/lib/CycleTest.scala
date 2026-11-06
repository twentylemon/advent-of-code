package org.lemon.advent.lib

import org.lemon.advent.*
import org.scalacheck.Gen
import org.scalacheck.Prop.*

class CycleTest extends UnitTest:

  // a random function from 0 until size to itself, and a state to start from
  val functionalGraph =
    for
      size <- Gen.choose(1, 60)
      next <- Gen.listOfN(size, Gen.choose(0, size - 1))
      initial <- Gen.choose(0, size - 1)
    yield (next.toVector, initial)

  test("findCycle and findCycleBy match a brute force search over random functional graphs") {
    check(forAll(functionalGraph) { (next: Vector[Int], initial: Int) =>
      // pigeonhole: some state must repeat within size + 1 steps
      val states = Iterator.iterate(initial)(next).take(next.size + 1).toVector
      val firstRepeat = states.indices.find(i => states.indexOf(states(i)) < i).get
      val offset = states.indexOf(states(firstRepeat))
      Seq(findCycle(initial)(next), findCycleBy(initial)(next)).forall(cycle =>
        cycle.offset == offset && cycle.period == firstRepeat - offset &&
          (0 until 200).forall(n => cycle.stateAt(n) == Iterator.iterate(initial)(next).nth(n))
      )
    })
  }

  test("findCycleBy finds the cycle in the keys") {
    // the step count never repeats, so the cycle is only found through the key
    check(forAll(functionalGraph) { (next: Vector[Int], initial: Int) =>
      val byKey = findCycleBy((initial, 0))((n, steps) => (next(n), steps + 1), _._1)
      val plain = findCycleBy(initial)(next)
      byKey.offset == plain.offset && byKey.period == plain.period && byKey.history.map(_._1) == plain.history
    })
  }

  test("findCycle detects simple cycle with no offset") {
    // 0 -> 1 -> 2 -> 0 -> 1 -> 2 -> ...
    val cycle = findCycle(0)(n => (n + 1) % 3)
    cycle shouldBe Cycle(offset = 0, period = 3, history = Seq(0, 1, 2))
  }

  test("findCycle detects cycle with offset") {
    // 0 -> 1 -> 2 -> 3 -> 4 -> 2 -> 3 -> 4 -> 2 -> ...
    val cycle = findCycle(0)(n => if n < 4 then n + 1 else if n == 4 then 2 else n + 1)
    cycle shouldBe Cycle(offset = 2, period = 3, history = Seq(0, 1, 2, 3, 4))
  }

  test("findCycle works with non-numeric state") {
    // a -> b -> c -> d -> b -> c -> d -> ...
    val transitions = Map("a" -> "b", "b" -> "c", "c" -> "d", "d" -> "b")
    val cycle = findCycle("a")(transitions)
    cycle shouldBe Cycle(offset = 1, period = 3, history = Seq("a", "b", "c", "d"))
    cycle.stateAt(0) shouldBe "a"
    cycle.stateAt(1) shouldBe "b"
    cycle.stateAt(100) shouldBe "b" // (100 - 1) % 3 = 0 -> position 0 in cycle -> history(1) = "b"
  }

  test("findCycle with fixed points") {
    // 0 -> 1 -> 2 -> 2 -> 2 -> ...
    val cycle = findCycle(0)(n => if n < 2 then n + 1 else 2)
    cycle shouldBe Cycle(offset = 2, period = 1, history = Seq(0, 1, 2))
    cycle.stateAt(100) shouldBe 2
  }

  test("stateAt returns correct state before cycle") {
    val cycle = findCycle(0)(n => if n < 4 then n + 1 else if n == 4 then 2 else n + 1)
    cycle.stateAt(0) shouldBe 0
    cycle.stateAt(1) shouldBe 1
  }

  test("stateAt returns correct state in cycle") {
    val cycle = findCycle(0)(n => if n < 4 then n + 1 else if n == 4 then 2 else n + 1)
    cycle.stateAt(2) shouldBe 2
    cycle.stateAt(3) shouldBe 3
    cycle.stateAt(4) shouldBe 4
  }

  test("stateAt returns correct state beyond history using cycle") {
    val cycle = findCycle(0)(n => if n < 4 then n + 1 else if n == 4 then 2 else n + 1)
    // offset=2, period=3, so steps 5,6,7 = steps 2,3,4
    cycle.stateAt(5) shouldBe 2
    cycle.stateAt(6) shouldBe 3
    cycle.stateAt(7) shouldBe 4
    cycle.stateAt(8) shouldBe 2
    // 1 billion steps: position in cycle = (1000000000 - 2) % 3 = 2, so state is history(4) = 4
    cycle.stateAt(1000000000L) shouldBe 4
    cycle.stateAt(1000000001L) shouldBe 2 // position 0 in cycle
    cycle.stateAt(1000000002L) shouldBe 3 // position 1 in cycle
  }
