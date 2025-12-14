package org.lemon.advent.lib

import scala.collection.mutable

/** Memoizes a function, caching the results of previous calls.
  * Use as:
  * ```scala
  * lazy val fib: BigInt => BigInt = memoize {
  *  case 0 => 0
  *  case 1 => 1
  *  case n => fib(n - 1) + fib(n - 2)
  * }
  * fib(100) shouldBe BigInt("354224848179261915075")
  * ```
  *
  * @param f the function to memoize
  * @return a memoized version of the function
  */
def memoize[T, R](f: T => R): T => R =
  val cache = mutable.Map.empty[T, R]
  (t: T) => cache.getOrElseUpdate(t, f(t))

def memoize[T1, T2, R](f: (T1, T2) => R): (T1, T2) => R =
  val cache = mutable.Map.empty[(T1, T2), R]
  (t1: T1, t2: T2) => cache.getOrElseUpdate((t1, t2), f(t1, t2))

def memoize[T1, T2, T3, R](f: (T1, T2, T3) => R): (T1, T2, T3) => R =
  val cache = mutable.Map.empty[(T1, T2, T3), R]
  (t1: T1, t2: T2, t3: T3) => cache.getOrElseUpdate((t1, t2, t3), f(t1, t2, t3))

def memoize[T1, T2, T3, T4, R](f: (T1, T2, T3, T4) => R): (T1, T2, T3, T4) => R =
  val cache = mutable.Map.empty[(T1, T2, T3, T4), R]
  (t1: T1, t2: T2, t3: T3, t4: T4) => cache.getOrElseUpdate((t1, t2, t3, t4), f(t1, t2, t3, t4))
