package org.lemon.advent.lib.parse

import org.lemon.advent._
import org.scalacheck.Prop._
import org.scalacheck._
import org.scalacheck.Arbitrary._

import org.lemon.advent.lib.parse.{given, _}

class CsvTest extends UnitTest:

  given Arbitrary[Seq[String]] = Arbitrary(Gen.nonEmptyListOf(Gen.alphaNumStr.suchThat(_.nonEmpty)))

  test("csv matches basic string") {
    check((xs: Seq[String]) =>
      xs.nonEmpty ==> {
        xs.mkString(",") match {
          case Csv[String](seq*) => seq == xs
          case _ => false
        }
      }
    )
  }

  test("csv matches with leading whitespace") {
    check((xs: Seq[String]) =>
      xs.nonEmpty ==> {
        xs.mkString(", ") match {
          case Csv[String](seq*) => seq == xs
          case _ => false
        }
      }
    )
  }

  test("csv matches with trailing whitespace") {
    check((xs: Seq[String]) =>
      xs.nonEmpty ==> {
        xs.mkString(", ") match {
          case Csv[String](seq*) => seq == xs
          case _ => false
        }
      }
    )
  }

  test("csv matches nested conversion") {
    check((xs: Seq[Int]) =>
      xs.mkString(",") match {
        case Csv[Int](seq*) => seq == xs
        case _ => false
      }
    )
  }

  test("csv matches nested conversion with whitespace") {
    check((xs: Seq[Int]) =>
      xs.mkString(" , ") match {
        case Csv[Int](seq*) => seq == xs
        case _ => false
      }
    )
  }
