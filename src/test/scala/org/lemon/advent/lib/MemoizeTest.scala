package org.lemon.advent.lib

import org.lemon.advent.UnitTest
import org.scalacheck.Gen
import org.scalacheck.Prop.*

import scala.collection.mutable

class MemoizeTest extends UnitTest:

  test("sanity: no memorize does not cache repeated calls") {
    var x = 0
    val f: Int => Int = _ => {
      x = x + 1
      x
    }
    f(0) shouldBe 1
    f(0) shouldBe 2
  }

  test("memoize caches repeated calls") {
    var x = 0
    val f: Int => Int = memoize { _ =>
      x = x + 1
      x
    }
    f(0) shouldBe 1
    f(0) shouldBe 1
  }

  val cacheSize = Gen.choose(1, 10)
  val keys = Gen.listOf(Gen.choose(0, 20))

  test("memoize with a max cache size gives the same results as the function") {
    check(forAll(cacheSize, keys) { (maxCacheSize: Int, keys: List[Int]) =>
      val f = (n: Int) => n * 31 + 7
      val cached = memoize(maxCacheSize)(f)
      maxCacheSize > 0 ==> keys.forall(k => cached(k) == f(k))
    })
  }

  test("memoize with a max cache size only recomputes the least recently used results") {
    check(forAll(cacheSize, keys) { (maxCacheSize: Int, keys: List[Int]) =>
      maxCacheSize > 0 ==> {
        val computed = mutable.Buffer.empty[Int]
        val cached = memoize(maxCacheSize)((n: Int) => { computed += n; n })
        keys.foreach(cached)

        // a reference LRU cache, most recently used first, recording each key it has to compute
        val (_, expected) = keys.foldLeft((List.empty[Int], Vector.empty[Int])) { case ((lru, misses), k) =>
          if lru.contains(k) then (k :: lru.filterNot(_ == k), misses)
          else ((k :: lru).take(maxCacheSize), misses :+ k)
        }
        computed == expected
      }
    })
  }

  test("memoize with a max cache size works for recursive functions") {
    check(forAll(Gen.choose(1, 5), Gen.choose(0, 25)) { (maxCacheSize: Int, n: Int) =>
      lazy val fib: Int => Long = memoize(maxCacheSize)(n => if n < 2 then n.toLong else fib(n - 1) + fib(n - 2))
      maxCacheSize > 0 ==> (fib(n) == Iterator.iterate((0L, 1L))((a, b) => (b, a + b)).nth(n)._1)
    })
  }

  test("memoize with room for three results computes each fibonacci number once") {
    // from fib(2) up, every fib(0) to fib(n) is needed
    check(forAll(Gen.choose(3, 10), Gen.choose(2, 200)) { (maxCacheSize: Int, n: Int) =>
      (maxCacheSize >= 3 && n >= 2) ==> {
        var calls = 0
        lazy val fib: Int => BigInt = memoize(maxCacheSize) { n =>
          calls += 1
          if n < 2 then BigInt(n) else fib(n - 1) + fib(n - 2)
        }
        fib(n)
        calls == n + 1
      }
    })
  }

  test("memoize with a max cache size caches by every argument") {
    check((a: Int, b: Int, c: Int, d: Int) =>
      var calls = 0
      val f2 = memoize(1)((w: Int, x: Int) => { calls += 1; w - x })
      val f3 = memoize(1)((w: Int, x: Int, y: Int) => { calls += 1; w - x - y })
      val f4 = memoize(1)((w: Int, x: Int, y: Int, z: Int) => { calls += 1; w - x - y - z })
      Seq(f2(a, b), f2(a, b), f3(a, b, c), f3(a, b, c), f4(a, b, c, d), f4(a, b, c, d)) ==
        Seq(a - b, a - b, a - b - c, a - b - c, a - b - c - d, a - b - c - d) && calls == 3
    )
  }

  test("memoize with a max cache size works with pattern matching functions") {
    lazy val fib: BigInt => BigInt = memoize(maxCacheSize = 3) {
      case 0 => 0
      case 1 => 1
      case n => fib(n - 1) + fib(n - 2)
    }
    lazy val paths: (Int, Int, Int) => Long = memoize(maxCacheSize = 100) {
      case (_, _, 0) => 1L
      case (x, y, steps) => paths(x + 1, y, steps - 1) + paths(x, y + 1, steps - 1)
    }
    fib(100) shouldBe BigInt("354224848179261915075")
    paths(0, 0, 20) shouldBe 1L << 20
  }
