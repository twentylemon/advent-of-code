package org.lemon.advent.lib

def gcd(a: Long, b: Long): Long = BigInt(a).gcd(BigInt(b)).toLong

def lcm(a: Long, b: Long) = a * b / gcd(a, b)

extension (x: Int)
  def +%(n: Int): Int =
    val mod = x % n
    if mod < 0 then mod + n else mod
