package org.lemon.advent.lib

import scala.math.BigInt

object Chunk:
  def unapplySeq(str: String): Option[Seq[String]] = Some(str.split("\n\n").toSeq)

object Num:
  def unapply[T: Numeric](s: String): Option[T] = summon[Numeric[T]].parseString(s)

// givens for delimiter-separated values to be generic
given (String => Option[String]) = Some(_)
given (String => Option[Int]) = Int.unapply
given (String => Option[Long]) = Long.unapply
given (String => Option[BigInt]) = BigInt.unapply

object Csv:
  private val delimeter = ","
  def unapplySeq[T](str: String)(using unapply: String => Option[T]): Option[Seq[T]] =
    Some(splitParse(str, delimeter).flatMap(unapply))

object Wsv:
  private val delimeter = "\\s+"
  def unapplySeq[T](str: String)(using unapply: String => Option[T]): Option[Seq[T]] =
    Some(splitParse(str, delimeter).flatMap(unapply))

private def splitParse(str: String, delimeter: String): Seq[String] =
  str.split(delimeter).map(_.trim).toSeq

// note: to extend existing types, the extensions need to be scoped in an implicit object
// object so that the functions created aren't top-level (and considered overloads)
// implicit so `import package._` brings them in
// https://github.com/scala/scala3/discussions/17660
implicit object ParseNumeric:
  extension (int: Int.type)
    def unapply(str: String): Option[Int] = Num.unapply(str)

  extension (long: Long.type)
    def unapply(str: String): Option[Long] = Num.unapply(str)

  extension (bigint: BigInt.type)
    def unapply(str: String): Option[BigInt] = Num.unapply(str)
