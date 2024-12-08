package org.lemon.advent.lib

def gcd(a: Long, b: Long): Long = BigInt(a).gcd(BigInt(b)).toLong

def lcm(a: Long, b: Long) = a * b / gcd(a, b)

extension (x: Int)
  def +%(n: Int): Int =
    val mod = x % n
    if mod < 0 then mod + n else mod

  def gcd(y: Int): Int = org.lemon.advent.lib.gcd(x.toLong, y.toLong).toInt
  def lcm(y: Int): Int = org.lemon.advent.lib.lcm(x.toLong, y.toLong).toInt
