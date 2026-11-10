package org.lemon.advent.lib

import scala.annotation.tailrec

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

  val hareStart = Iterator.iterate(initial)(step).nth(period)
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

/** Detects a cycle in the sequence of states produced by repeatedly applying `step` to `initial`,
  * where two states are the same when they have equal keys. This implementation is faster when
  * `step` is expensive. [[findCycle]] can be faster when `step` is cheap and comparing two states
  * usually stops early.
  *
  * @param initial the starting state
  * @param step function to produce the next state
  * @param key function giving the part of a state that determines the states after it
  * @return the cycle information including offset, period, and state lookup
  * @tparam S the state type
  * @tparam K the key type
  */
def findCycleBy[S, K](initial: S)(step: S => S, key: S => K): Cycle[S] =
  @tailrec
  def loop(state: S, seen: Map[K, Int], history: Vector[S]): Cycle[S] =
    val k = key(state)
    seen.get(k) match
      case Some(offset) => Cycle(offset, history.size - offset, history)
      case None => loop(step(state), seen.updated(k, history.size), history :+ state)
  loop(initial, Map.empty, Vector.empty)

/** Detects a cycle in the sequence of states produced by repeatedly applying `step` to `initial`,
  * by hashing the states.
  *
  * @param initial the starting state
  * @param step function to produce the next state
  * @return the cycle information including offset, period, and state lookup
  * @tparam S the state type
  */
def findCycleBy[S](initial: S)(step: S => S): Cycle[S] =
  findCycleBy(initial)(step, identity)
