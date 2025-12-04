package org.lemon.advent.lib

import org.lemon.advent._

class CycleTest extends UnitTest:

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
    cycle.stateAt(100) shouldBe "b"  // (100 - 1) % 3 = 0 -> position 0 in cycle -> history(1) = "b"
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
    cycle.stateAt(1000000001L) shouldBe 2  // position 0 in cycle
    cycle.stateAt(1000000002L) shouldBe 3  // position 1 in cycle
  }
