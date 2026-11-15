package org.lemon.advent.lib

import org.scalacheck.Gen

def amountWithin(lo: Long, hi: Long): Gen[Int] = Gen.chooseNum(lo.max(Int.MinValue).toInt, hi.min(Int.MaxValue).toInt)
def roomBelow(x: Int): Long = x.toLong - Int.MinValue
def roomAbove(x: Int): Long = Int.MaxValue.toLong - x
