package org.lemon.advent

import scala.annotation.tailrec

@tailrec
def gcd(a: Long, b: Long): Long = if b == 0 then a else gcd(b, a % b)

def lcm(a: Long, b: Long) = a * b / gcd(a, b)
