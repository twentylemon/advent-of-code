package org.lemon.advent.lib.parse

import org.lemon.advent.*
import org.scalacheck.Prop.*
import org.scalacheck.*
import org.scalacheck.Arbitrary.*

class CsvTest extends UnitTest:

  given Arbitrary[Seq[String]] = Arbitrary(Gen.nonEmptyListOf(Gen.alphaNumStr.suchThat(_.nonEmpty)))

  test("csv matches basic string") {
    check((xs: Seq[String]) =>
      xs.nonEmpty ==> {
        xs.mkString(",") match {
          case Csv(seq*) => seq == xs
          case _ => false
        }
      }
    )
  }

  test("csv matches with leading whitespace") {
    check((xs: Seq[String]) =>
      xs.nonEmpty ==> {
        xs.mkString(", ") match {
          case Csv(seq*) => seq == xs
          case _ => false
        }
      }
    )
  }

  test("csv matches with trailing whitespace") {
    check((xs: Seq[String]) =>
      xs.nonEmpty ==> {
        xs.mkString(", ") match {
          case Csv(seq*) => seq == xs
          case _ => false
        }
      }
    )
  }
