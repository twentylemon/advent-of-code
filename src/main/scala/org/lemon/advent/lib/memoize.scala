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

/** Memoizes a function, keeping at most `maxCacheSize` results and evicting the least recently used.
  * An evicted result is computed again the next time it is needed, so a recursive function needs room
  * for the results it is still using, or it can take exponential time. Can be called from several
  * threads, though two threads may compute the same result.
  * Use as:
  * ```scala
  * lazy val fib: BigInt => BigInt = memoize(maxCacheSize = 3) {
  *  case 0 => 0
  *  case 1 => 1
  *  case n => fib(n - 1) + fib(n - 2)
  * }
  * fib(100) shouldBe BigInt("354224848179261915075")
  * ```
  *
  * @param maxCacheSize the most results to keep
  * @param f the function to memoize
  * @return a memoized version of the function
  */
def memoize[T, R](maxCacheSize: Int)(f: T => R): T => R =
  require(maxCacheSize > 0, s"maxCacheSize must be positive, was $maxCacheSize")
  // in access order, so the eldest entry is the least recently used
  val cache = new java.util.LinkedHashMap[T, R](16, 0.75f, true):
    override def removeEldestEntry(eldest: java.util.Map.Entry[T, R]): Boolean = size > maxCacheSize

  (t: T) =>
    cache.synchronized(Option.when(cache.containsKey(t))(cache.get(t))).getOrElse {
      // computed outside the lock, since f may call back into the cache
      val result = f(t)
      cache.synchronized(cache.put(t, result))
      result
    }

def memoize[T1, T2, R](maxCacheSize: Int)(f: (T1, T2) => R): (T1, T2) => R =
  val cached = memoize(maxCacheSize)(f.tupled)
  (t1: T1, t2: T2) => cached((t1, t2))

def memoize[T1, T2, T3, R](maxCacheSize: Int)(f: (T1, T2, T3) => R): (T1, T2, T3) => R =
  val cached = memoize(maxCacheSize)(f.tupled)
  (t1: T1, t2: T2, t3: T3) => cached((t1, t2, t3))

def memoize[T1, T2, T3, T4, R](maxCacheSize: Int)(f: (T1, T2, T3, T4) => R): (T1, T2, T3, T4) => R =
  val cached = memoize(maxCacheSize)(f.tupled)
  (t1: T1, t2: T2, t3: T3, t4: T4) => cached((t1, t2, t3, t4))
