package org.lemon.advent.lib

extension [A](it: Iterator[A])
  def nth(n: Int): A = it.drop(n).next
  def nthOption(n: Int): Option[A] = it.drop(n).nextOption
