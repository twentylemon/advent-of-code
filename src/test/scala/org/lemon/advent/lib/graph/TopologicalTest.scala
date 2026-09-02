package org.lemon.advent.lib.graph

import org.lemon.advent.*
import org.scalacheck.*
import org.scalacheck.Prop.*

class TopologicalTest extends UnitTest:

  test("empty graph yields empty ordering") {
    topologicalSort(Map.empty[Int, Iterable[Int]]) shouldBe Some(Seq.empty[Int])
  }

  test("single node with no edges") {
    topologicalSort(Map(1 -> Seq.empty[Int])) shouldBe Some(Seq(1))
  }

  test("simple linear chain is sorted in dependency order") {
    val graph = Map(1 -> Seq(2), 2 -> Seq(3), 3 -> Seq(4), 4 -> Seq.empty[Int])
    topologicalSort(graph) shouldBe Some(Seq(1, 2, 3, 4))
  }

  test("respects iteration order when nodes have no edges") {
    val nodes = Seq(3, 1, 2, 4)
    topologicalSort(nodes, _ => Seq.empty[Int]) shouldBe Some(Seq(3, 1, 2, 4))
  }

  test("diamond shape produces a valid ordering") {
    // 1 -> 2, 1 -> 3, 2 -> 4, 3 -> 4
    val graph = Map(1 -> Seq(2, 3), 2 -> Seq(4), 3 -> Seq(4), 4 -> Seq.empty[Int])
    val sorted = topologicalSort(graph).get
    sorted should contain theSameElementsAs Seq(1, 2, 3, 4)
    sorted.indexOf(1) should be < sorted.indexOf(2)
    sorted.indexOf(1) should be < sorted.indexOf(3)
    sorted.indexOf(2) should be < sorted.indexOf(4)
    sorted.indexOf(3) should be < sorted.indexOf(4)
  }

  test("simple cycle returns None") {
    val graph = Map(1 -> Seq(2), 2 -> Seq(3), 3 -> Seq(1))
    topologicalSort(graph) shouldBe None
  }

  test("self loop returns None") {
    val graph = Map(1 -> Seq(1))
    topologicalSort(graph) shouldBe None
  }

  test("ignores edges to nodes outside the requested subset") {
    // graph: 1 -> 2 -> 3, but only ask for {1, 3} — 1 has no in-graph successor
    val adjacency: Int => Seq[Int] = Map(1 -> Seq(2), 2 -> Seq(3), 3 -> Seq.empty)
    val sorted = topologicalSort(Seq(1, 3), adjacency).get
    sorted should contain theSameElementsAs Seq(1, 3)
  }

  test("cycle in unrelated subgraph does not affect requested subset") {
    // 4 <-> 5 cycle exists but we only ask for {1, 2, 3} which is acyclic
    val adjacency: Int => Seq[Int] =
      Map(1 -> Seq(2), 2 -> Seq(3), 3 -> Seq.empty, 4 -> Seq(5), 5 -> Seq(4))
    topologicalSort(Seq(1, 2, 3), adjacency) shouldBe Some(Seq(1, 2, 3))
  }

  test("duplicate nodes are deduplicated while preserving first-seen order") {
    val sorted = topologicalSort(Seq(2, 1, 2, 3, 1), (_: Int) => Seq.empty[Int]).get
    sorted shouldBe Seq(2, 1, 3)
  }

  test("ordering satisfies every adjacency edge") {
    val nodeGen = Gen.choose(0, 50)
    val edgeGen = for
      a <- nodeGen
      b <- nodeGen.suchThat(_ != a)
    yield (a, b)
    val dagGen = for
      n <- Gen.choose(0, 30)
      pairs <- Gen.listOfN(n, edgeGen)
      // make it acyclic by enforcing edges go from low to high node
    yield pairs.map { case (a, b) => if a < b then (a, b) else (b, a) }.distinct

    check(forAll(dagGen) { edges =>
      val nodes = (edges.flatMap { case (a, b) => Seq(a, b) }).distinct
      val adjacency = edges.groupMap(_._1)(_._2).withDefaultValue(Seq.empty[Int])
      topologicalSort(nodes, adjacency) match
        case None => false
        case Some(sorted) =>
          sorted.toSet == nodes.toSet &&
          edges.forall { case (a, b) => sorted.indexOf(a) < sorted.indexOf(b) }
    })
  }

  test("any cycle is detected") {
    // build a guaranteed cycle: a forward chain 0 -> 1 -> ... -> n plus a back-edge n -> 0,
    // optionally sprinkled with extra forward edges
    val gen = for
      n <- Gen.choose(1, 20)
      extra <- Gen.listOf(for
        a <- Gen.choose(0, n)
        b <- Gen.choose(0, n).suchThat(_ != a)
      yield if a < b then (a, b) else (b, a))
    yield ((0 until n).map(i => (i, i + 1)) :+ (n, 0)) ++ extra

    check(forAll(gen) { edges =>
      val nodes = edges.flatMap { case (a, b) => Seq(a, b) }.distinct
      val adjacency = edges.groupMap(_._1)(_._2).withDefaultValue(Seq.empty[Int])
      topologicalSort(nodes, adjacency).isEmpty
    })
  }
