package org.lemon.advent.lib

import scala.collection.mutable

def memoize[T, R](f: T => R): T => R =
  val cache = mutable.Map.empty[T, R]
  (t: T) => cache.getOrElseUpdate(t, f(t))

def memoize[T1, T2, R](f: (T1, T2) => R): (T1, T2) => R =
  val cache = mutable.Map.empty[(T1, T2), R]
  (t1: T1, t2: T2) => cache.getOrElseUpdate((t1, t2), f(t1, t2))

def memoize[T1, T2, T3, R](f: (T1, T2, T3) => R): (T1, T2, T3) => R =
  val cache = mutable.Map.empty[(T1, T2, T3), R]
  (t1: T1, t2: T2, t3: T3) => cache.getOrElseUpdate((t1, t2, t3), f(t1, t2, t3))
