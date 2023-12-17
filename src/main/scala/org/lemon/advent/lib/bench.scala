package org.lemon.advent.lib

import org.lemon.advent._
import org.lemon.advent.lib._
import org.openjdk.jmh.annotations.Benchmark

class Bench:

  @Benchmark // Jmh/run org.lemon.advent.lib.Bench
  def bench(): Unit =
    // import org.lemon.advent.year2023.Day16._
    // part1(read(file(2023)(16)))
    
    Area(0 to 100, 0 to 100).toSeq

    ()
