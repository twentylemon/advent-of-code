package org.lemon.advent.bench

import org.lemon.advent.*
import org.lemon.advent.lib.*
import org.lemon.advent.lib.`2d`.*
import org.lemon.advent.lib.graph
import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole

import java.util.concurrent.TimeUnit

/* Each class times the current library code against candidates from `candidates.scala` (one using
 * local mutable state like the library does, and one using only vals and immutable collections), on real
 * puzzle inputs where possible. `@Setup` checks both give the same answer first. Run one with:
 *
 *   Jmh/run org.lemon.advent.bench.PathFindBench
 *
 * Defaults below are sized for a small machine; override with eg `-wi 5 -i 10 -f 2`.
 */

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
// the candidate takes much longer to reach steady state on a slow 2 core machine; short warmups make it look 5x slower
@Warmup(iterations = 10, time = 1)
@Measurement(iterations = 5, time = 1)
@Fork(value = 1, jvmArgsAppend = Array("-Xmx2g"))
class IntervalIterationBench:
  val interval: Interval[Int] = Interval(0, 100_000)
  val area: Area = Area(0 until 141, 0 until 141)

  @Setup def verify(): Unit =
    require(interval.iterator.sameElements(candidates.IntervalIterator(interval.start, interval.end)))
    require((area: Iterator[Coord]).sameElements(candidates.rectIterator(area)))
    require(interval.iterator.sameElements(immutableCandidates.intervalIterator(interval.start, interval.end)))
    require((area: Iterator[Coord]).sameElements(immutableCandidates.rectIterator(area)))

  @Benchmark def intervalCurrent(bh: Blackhole): Unit = interval.iterator.foreach(bh.consume(_))
  @Benchmark def intervalCandidate(bh: Blackhole): Unit =
    candidates.IntervalIterator(interval.start, interval.end).foreach(bh.consume(_))

  @Benchmark def areaCurrent(bh: Blackhole): Unit = (area: Iterator[Coord]).foreach(bh.consume(_))
  @Benchmark def areaCandidate(bh: Blackhole): Unit = candidates.rectIterator(area).foreach(bh.consume(_))

  @Benchmark def intervalImmutable(bh: Blackhole): Unit =
    immutableCandidates.intervalIterator(interval.start, interval.end).foreach(bh.consume(_))
  @Benchmark def areaImmutable(bh: Blackhole): Unit = immutableCandidates.rectIterator(area).foreach(bh.consume(_))

case class Crucible(at: Coord, direction: Direction, forward: Int)

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 2, time = 2)
@Measurement(iterations = 3, time = 2)
@Fork(value = 1, jvmArgsAppend = Array("-Xmx2g"))
class PathFindBench:
  // 2023 day 17 part 1: A* over (coord, direction, steps forward) on the real input
  val heat: Map[Coord, Int] = Coord.gridToMap(read(file(2023)(17))).view.mapValues(_.asDigit).toMap
  val lavaPool: Coord = Area(heat).bottomRight
  val crucibleStart = Crucible(Coord.origin, Direction.Right, 0)
  def crucible(state: Crucible): Seq[(Crucible, Int)] =
    val turns = Seq(state.direction.turnLeft, state.direction.turnRight).map(d => Crucible(state.at + d, d, 1))
    val forward = Option.when(state.forward < 3)(state.copy(at = state.at + state.direction, forward = state.forward + 1))
    (turns ++ forward).filter(s => heat.contains(s.at)).map(s => s -> heat(s.at))
  def crucibleHeuristic(state: Crucible): Int = state.at.manhattan(lavaPool)

  // 2024 day 18 part 1: unit cost maze on the real input
  val corrupt: Set[Coord] = read(file(2024)(18)).linesIterator.take(1024).map { case s"$x,$y" => Coord(x.toInt, y.toInt) }.toSet
  val memory: Area = Area(0 to 70, 0 to 70)
  def maze(coord: Coord): Seq[(Coord, Int)] = coord.adjacent.filterNot(corrupt).filter(memory.contains).map((_, 1))

  @Setup def verify(): Unit =
    require(crucibleCurrent() == crucibleCandidate(), "crucible distances differ")
    require(mazeCurrent() == mazeCandidate(), "maze distances differ")
    require(crucibleCurrent() == crucibleImmutable(), "immutable crucible distances differ")
    require(mazeCurrent() == mazeImmutable(), "immutable maze distances differ")

  @Benchmark def crucibleCurrent(): Int =
    graph.pathFind(crucible, crucibleHeuristic, crucibleStart, _.at == lavaPool).get.distance
  @Benchmark def crucibleCandidate(): Int =
    candidates.pathFind(crucible, crucibleHeuristic, crucibleStart, _.at == lavaPool).get.distance

  @Benchmark def mazeCurrent(): Int =
    graph.pathFind(maze, (_: Coord) => 0, Coord.origin, _ == memory.bottomRight).get.distance
  @Benchmark def mazeCandidate(): Int =
    candidates.pathFind(maze, (_: Coord) => 0, Coord.origin, _ == memory.bottomRight).get.distance

  @Benchmark def crucibleImmutable(): Int =
    immutableCandidates.pathFind(crucible, crucibleHeuristic, crucibleStart, _.at == lavaPool).get.distance
  @Benchmark def mazeImmutable(): Int =
    immutableCandidates.pathFind(maze, (_: Coord) => 0, Coord.origin, _ == memory.bottomRight).get.distance

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 1, time = 2)
@Measurement(iterations = 3, time = 2)
@Fork(value = 1, jvmArgsAppend = Array("-Xmx2g"))
class CompressBench:
  def corridors(grid: Map[Coord, Char]): Map[Coord, Seq[Coord]] =
    grid.filter(_._2 != '#').map((c, _) => c -> c.adjacent.filter(n => grid.getOrElse(n, '#') != '#'))

  // 2016 day 24: junctions plus numbered checkpoints, on the real input
  val ducts: Map[Coord, Char] = Coord.gridToMap(read(file(2016)(24)))
  val ductCorridors: Map[Coord, Seq[Coord]] = corridors(ducts)
  val ductNodes: Set[Coord] = graph.junctions(ductCorridors) ++ ducts.filter(_._2.isDigit).keys
  def ductAdjacency(coord: Coord): Seq[(Coord, Int)] = ductCorridors(coord).map((_, 1))

  // 2023 day 23 part 2: junctions plus start and end, on the real input
  val trails: Map[Coord, Char] = Coord.gridToMap(read(file(2023)(23)))
  val trailCorridors: Map[Coord, Seq[Coord]] = corridors(trails)
  val trailNodes: Set[Coord] =
    graph.junctions(trailCorridors) + Area(trails).topRow.find(trails(_) == '.').get +
      Area(trails).bottomRow.find(trails(_) == '.').get
  def trailAdjacency(coord: Coord): Seq[(Coord, Int)] = trailCorridors(coord).map((_, 1))

  @Setup def verify(): Unit =
    println(s"\nimportant nodes: ducts=${ductNodes.size} trails=${trailNodes.size}")
    require(ductsCurrent() == ductsCandidate(), "duct graphs differ")
    require(trailsCurrent() == trailsCandidate(), "trail graphs differ")
    require(ductsCurrent() == ductsImmutable(), "immutable duct graphs differ")
    require(trailsCurrent() == trailsImmutable(), "immutable trail graphs differ")

  @Benchmark def ductsCurrent(): graph.WeightedGraph[Coord, Int] = graph.compress(ductAdjacency, ductNodes)
  @Benchmark def ductsCandidate(): graph.WeightedGraph[Coord, Int] = candidates.compress(ductAdjacency, ductNodes)

  @Benchmark def trailsCurrent(): graph.WeightedGraph[Coord, Int] = graph.compress(trailAdjacency, trailNodes)
  @Benchmark def trailsCandidate(): graph.WeightedGraph[Coord, Int] = candidates.compress(trailAdjacency, trailNodes)

  @Benchmark def ductsImmutable(): graph.WeightedGraph[Coord, Int] =
    immutableCandidates.compress(ductAdjacency, ductNodes)
  @Benchmark def trailsImmutable(): graph.WeightedGraph[Coord, Int] =
    immutableCandidates.compress(trailAdjacency, trailNodes)

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 2, time = 2)
@Measurement(iterations = 3, time = 2)
@Fork(value = 1, jvmArgsAppend = Array("-Xmx2g"))
class AllShortestPathsBench:
  // 2024 day 16 part 2 on the real input
  val maze: Map[Coord, Char] = Coord.gridToMap(read(file(2024)(16)))
  val start: (Coord, Direction) = (maze.findValue('S').get, Direction.Right)
  val end: Coord = maze.findValue('E').get
  def reindeer(state: (Coord, Direction)): Seq[((Coord, Direction), Int)] =
    val (coord, dir) = state
    val turns = Seq((coord, dir.turnLeft) -> 1000, (coord, dir.turnRight) -> 1000)
    if maze(coord + dir) == '#' then turns else turns :+ ((coord + dir, dir) -> 1)

  @Setup def verify(): Unit =
    val (current, candidate) = (
      graph.allShortestPaths(reindeer, start, _._1 == end),
      candidates.allShortestPaths(reindeer, start, _._1 == end),
    )
    println(s"\nshortest paths: ${current.size}")
    require(current == candidate, "paths differ")
    require(current == immutableCandidates.allShortestPaths(reindeer, start, _._1 == end), "immutable paths differ")

  @Benchmark def tilesCurrent(): Int =
    graph.allShortestPaths(reindeer, start, _._1 == end).flatMap(_.path.map(_._1)).size
  @Benchmark def tilesCandidate(): Int =
    candidates.allShortestPaths(reindeer, start, _._1 == end).flatMap(_.path.map(_._1)).size
  @Benchmark def tilesImmutable(): Int =
    immutableCandidates.allShortestPaths(reindeer, start, _._1 == end).flatMap(_.path.map(_._1)).size

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 1, time = 2)
@Measurement(iterations = 3, time = 2)
@Fork(value = 1, jvmArgsAppend = Array("-Xmx2g"))
class FindCycleBench:
  // 2023 day 14 part 2 on the real input: each step tilts the dish north, west, south and east
  val dish: Seq[Seq[Char]] = read(file(2023)(14)).linesIterator.map(_.toSeq).toSeq.map(_.reverse).transpose

  def tiltLeft(row: Seq[Char]): Seq[Char] = Seq.unfold(row)(row =>
    if row.isEmpty then None
    else
      val (walls, tail) = row.span(_ == '#')
      val (space, rest) = tail.span(_ != '#')
      val rocks = space.count(_ == 'O')
      Some((walls ++ Seq.fill(rocks)('O') ++ Seq.fill(space.length - rocks)('.'), rest))
  ).flatten
  def tiltAndRotate(grid: Seq[Seq[Char]]): Seq[Seq[Char]] = grid.map(tiltLeft).transpose.map(_.reverse)
  def spin(grid: Seq[Seq[Char]]): Seq[Seq[Char]] = Iterator.iterate(grid)(tiltAndRotate).nth(4)

  @Setup def verify(): Unit =
    var (currentSteps, candidateSteps) = (0, 0)
    val current = findCycle(dish)(g => { currentSteps += 1; spin(g) })
    val candidate = candidates.findCycle(dish)(g => { candidateSteps += 1; spin(g) })
    println(s"\noffset=${current.offset} period=${current.period}: " +
      s"current calls step $currentSteps times, candidate $candidateSteps times")
    require(current == candidate, "cycles differ")
    require(current == immutableCandidates.findCycle(dish)(spin), "immutable cycles differ")

  @Benchmark def spinCurrent(): Int = findCycle(dish)(spin).period
  @Benchmark def spinCandidate(): Int = candidates.findCycle(dish)(spin).period
  @Benchmark def spinImmutable(): Int = immutableCandidates.findCycle(dish)(spin).period
