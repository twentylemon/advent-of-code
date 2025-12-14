package org.lemon.advent.lib

case class Cycle[S](offset: Int, period: Int, history: Seq[S]):
  def stateAt(n: Long): S =
    if n < history.size then history(n.toInt)
    else history(((n - offset) % period + offset).toInt)

/** Detects a cycle in the sequence of states produced by repeatedly applying `step` to `initial`,
  * comparing states by using `eq`.
  *
  * @param initial the starting state
  * @param step function to produce the next state
  * @param eq function to compare if two states are equal
  * @return the cycle information including offset, period, and state lookup
  * @tparam S the state type
  */
def findCycle[S](initial: S)(step: S => S, eq: (S, S) => Boolean): Cycle[S] =
  case class BrentState(power: Int, period: Int, tortoise: S, hare: S)

  val period = Iterator.iterate(BrentState(1, 1, initial, step(initial))) { state =>
    val (power, period, tortoise) =
      if state.power == state.period then (state.power * 2, 1, state.hare)
      else (state.power, state.period + 1, state.tortoise)
    BrentState(power, period, tortoise, step(state.hare))
  }.find(s => eq(s.tortoise, s.hare)).map(_.period).get

  val hareStart = Iterator.iterate(initial)(step).drop(period).next()
  val (offset, historyPrefix, cycleStart) = Iterator
    .iterate((initial, hareStart, Vector.empty[S])) { (t, h, hist) => (step(t), step(h), hist :+ t) }
    .find((tortoise, hare, _) => eq(tortoise, hare))
    .map((tortoise, _, hist) => (hist.size, hist, tortoise))
    .get

  val history = historyPrefix ++ Iterator.iterate(cycleStart)(step).take(period).toSeq
  Cycle(offset, period, history)

/** Detects a cycle in the sequence of states produced by repeatedly applying `step` to `initial`.
  * Uses Brent's cycle detection algorithm.
  *
  * @param initial the starting state
  * @param step function to produce the next state
  * @return the cycle information including offset, period, and state lookup
  * @tparam S the state type
  */
def findCycle[S](initial: S)(step: S => S): Cycle[S] =
  findCycle(initial)(step, _ == _)
