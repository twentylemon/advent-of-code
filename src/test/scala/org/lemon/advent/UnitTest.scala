package org.lemon.advent

import org.scalatest.Tag
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.Checkers

abstract class UnitTest extends AnyFunSuite with Matchers with Checkers
