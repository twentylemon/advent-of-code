package org.lemon.advent.lib

import scala.annotation.tailrec

def gcd(a: Long, b: Long): Long = BigInt(a).gcd(BigInt(b)).toLong

def lcm(a: Long, b: Long) = a * b / gcd(a, b)
