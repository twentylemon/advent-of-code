package org.lemon.advent.year2023

import org.lemon.advent.lib.lcm

import scala.collection.mutable

private object Day20:

  sealed trait Circuit:
    def name: String
    def downstream: Seq[String]

    def connect(upstream: Circuit): Unit = ()
    def receive(signal: Boolean, from: Circuit): Option[Boolean]

  case class Broadcast(name: String, downstream: Seq[String]) extends Circuit:
    override def receive(signal: Boolean, from: Circuit) = Some(false)

  case class FlipFlip(name: String, downstream: Seq[String], private var state: Boolean = false) extends Circuit:
    override def receive(signal: Boolean, from: Circuit) =
      Option.when(!signal) { state = !state; state }

  case class Conjuction(
      name: String,
      downstream: Seq[String],
      private val state: mutable.Map[String, Boolean] = mutable.Map.empty
  ) extends Circuit:
    override def connect(upstream: Circuit): Unit = state += upstream.name -> false

    override def receive(signal: Boolean, from: Circuit) =
      state(from.name) = signal
      Some(if state.values.forall(x => x) then false else true)

  case class Output(name: String) extends Circuit:
    def downstream: Seq[String] = Seq.empty
    override def receive(signal: Boolean, from: Circuit) = None

  case class CircuitContext(circuits: Map[String, Circuit]):
    circuits.values.foreach(circuit => circuit.downstream.foreach(name => circuits(name).connect(circuit)))

  def parse(input: String) =
    val circuits = input.linesIterator
      .map(_ match
        case s"%$name -> $downstream" => FlipFlip(name = name, downstream = downstream.split(", "))
        case s"&$name -> $downstream" => Conjuction(name = name, downstream = downstream.split(", "))
        case s"broadcaster -> $downstream" => Broadcast(name = "broadcaster", downstream = downstream.split(", "))
      )
      .map(circuit => circuit.name -> circuit)
      .toMap + ("button" -> Broadcast(name = "button", downstream = Seq("broadcaster")))

    val expected = input.linesIterator.flatMap(_.split(" -> ").last.split(", ")).distinct.filterNot(circuits.contains)
    circuits ++ expected.map(Output(_)).map(output => output.name -> output).toMap

  def pressTheButton(context: CircuitContext, sniff: (Boolean, Circuit) => Any = (_, _) => ()): (Long, Long) =
    var (low, high) = (0L, 0L)
    val queue = mutable.Queue((false, context.circuits("button"), context.circuits("broadcaster")))

    while !queue.isEmpty do
      val (signal, from, to) = queue.dequeue
      sniff(signal, from)
      if signal then high = high + 1 else low = low + 1
      val next = to.receive(signal, from).map(s => to.downstream.map(name => (s, to, context.circuits(name))))
      queue ++= next.getOrElse(Seq.empty)
    (low, high)

  def resolve(circuits: Map[String, Circuit]) =
    val context = CircuitContext(circuits)

  def part1(input: String) =
    val context = CircuitContext(parse(input))
    val presses = for _ <- (0 until 1000).iterator yield pressTheButton(context)
    val score = presses.reduce { case ((l1, h1), (l2, h2)) => (l1 + l2, h1 + h2) }
    score._1 * score._2

  def part2(input: String): Long =
    val context = CircuitContext(parse(input))

    // my input has (&xl, &ln, &xp, &gp) -> &df -> rx
    val chain = Seq("xl", "ln", "xp", "gp")
    val counts = mutable.Map.empty[String, Long]

    try
      val presses =
        for
          press <- 1 until 100000
        yield pressTheButton(
          context,
          (signal, circuit) =>
            if signal && chain.contains(circuit.name) then
              counts.updateWith(circuit.name)(_.orElse(Some(press)))
              if counts.size == chain.size then
                throw new AssertionError(counts.values.fold(1L)(lcm).toString)
        )
      0L
    catch
      case success: AssertionError => success.getMessage().toLong
      case rip: Throwable => throw rip
