package org.lemon.advent.lib

import org.lemon.advent.UnitTest
import org.scalacheck.Gen
import org.scalacheck.Prop.*

class MemoizeTest extends UnitTest:

  test("memoize calls the function once for each distinct argument") {
    check((xs: List[Byte]) =>
      var calls = 0
      val f = memoize((a: Byte) => { calls += 1; a * 2 })
      xs.map(f) == xs.map(_ * 2) && calls == xs.distinct.size
    )
  }

  test("memoize of 2 arguments calls the function once for each distinct set of arguments") {
    check((xs: List[(Boolean, Byte)]) =>
      var calls = 0
      val f = memoize((a: Boolean, b: Byte) => { calls += 1; s"$a$b" })
      xs.map(f.tupled) == xs.map((a, b) => s"$a$b") && calls == xs.distinct.size
    )
  }

  test("memoize of 3 arguments calls the function once for each distinct set of arguments") {
    check((xs: List[(Boolean, Boolean, Byte)]) =>
      var calls = 0
      val f = memoize((a: Boolean, b: Boolean, c: Byte) => { calls += 1; s"$a$b$c" })
      xs.map(f.tupled) == xs.map((a, b, c) => s"$a$b$c") && calls == xs.distinct.size
    )
  }

  test("memoize of 4 arguments calls the function once for each distinct set of arguments") {
    check((xs: List[(Boolean, Boolean, Boolean, Byte)]) =>
      var calls = 0
      val f = memoize((a: Boolean, b: Boolean, c: Boolean, d: Byte) => { calls += 1; s"$a$b$c$d" })
      xs.map(f.tupled) == xs.map((a, b, c, d) => s"$a$b$c$d") && calls == xs.distinct.size
    )
  }

  test("memoize works on a recursive function") {
    check(forAll(Gen.choose(2, 200)) { n =>
      var calls = 0
      lazy val fib: Int => BigInt = memoize { k =>
        calls += 1
        if k < 2 then BigInt(k) else fib(k - 1) + fib(k - 2)
      }
      val expected = Iterator.iterate((BigInt(0), BigInt(1)))((a, b) => (b, a + b)).drop(n).next._1
      fib(n) == expected && calls == n + 1
    })
  }
