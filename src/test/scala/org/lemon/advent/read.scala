package org.lemon.advent

import scala.io.Source
import scala.util.Using

def read(name: String): String =
  Using.resource(Source.fromResource(name))(source => source.mkString)

def readLines(name: String): Seq[String] =
  Using.resource(Source.fromResource(name))(source => source.getLines.toSeq)

def file(year: Int)(day: Int) = s"year$year/day${"%02d".format(day)}.txt"
