package org.lemon.advent.lib

import org.lemon.advent.UnitTest

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
