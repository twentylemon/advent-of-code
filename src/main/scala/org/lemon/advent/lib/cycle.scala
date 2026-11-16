package org.lemon.advent.lib

import scala.annotation.tailrec

/** A sequence of states that eventually repeats: after `offset` states, the next `period` states repeat forever.
  * @param offset the index of the first state in the cycle
  * @param period the length of the cycle
  * @param history the first `offset + period` states
  * @tparam S the state type
  */
case class Cycle[S](offset: Int, period: Int, history: Seq[S]):
  def stateAt(n: Long): S =
    if n < history.size then history(n.toInt)
    else history(((n - offset) % period + offset).toInt)

/** Detects a cycle in the sequence of states produced by repeatedly applying `step` to `initial`,
  * comparing states by using `eq`. Uses Brent's cycle detection algorithm.
  *
  * @param initial the starting state
  * @param step function to produce the next state
  * @param eq function to compare if two states are equal
  * @return the cycle information including offset, period, and state lookup
  * @tparam S the state type
  */
def findCycle[S](initial: S)(step: S => S, eq: (S, S) => Boolean): Cycle[S] =
  @tailrec
  def brent(power: Int, period: Int, tortoise: S, states: List[S]): (Int, List[S]) =
    val hare = states.head
    if eq(tortoise, hare) then (period, states)
    else if power == period then brent(power * 2, 1, hare, step(hare) :: states)
    else brent(power, period + 1, tortoise, step(hare) :: states)

  val (period, reversed) = brent(1, 1, initial, List(step(initial), initial))
  val history = reversed.reverse.toVector
  val offset = history.indices.find(i => eq(history(i), history(i + period))).get
  Cycle(offset, period, history.take(offset + period))

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
