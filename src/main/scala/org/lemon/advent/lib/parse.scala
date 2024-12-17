package org.lemon.advent.lib

object Chunk:
  def unapplySeq(s: String): Option[Seq[String]] = Some(s.split("\n\n").toSeq)

object Csv:
  private val delimeter = ","
  def unapply[T: Numeric](s: String): Option[Seq[T]] = Some(splitParse(s, delimeter).flatMap(Num.unapply[T]))
  def unapply(s: String): Option[Seq[String]] = Some(splitParse(s, delimeter))

object Wsv:
  private val delimeter = "\\s+"
  def unapply[T: Numeric](s: String): Option[Seq[T]] = Some(splitParse(s, delimeter).flatMap(Num.unapply[T]))
  def unapply(s: String): Option[Seq[String]] = Some(splitParse(s, delimeter))

extension (int: Int.type)
  def unapply(s: String): Option[Int] = Num.unapply(s)

extension (long: Long.type)
  def unapply(s: String): Option[Long] = Num.unapply(s)

extension (bigint: BigInt.type)
  def unapply(s: String): Option[BigInt] = Num.unapply(s)

object Num:
  def unapply[T: Numeric](s: String): Option[T] = summon[Numeric[T]].parseString(s)

private def splitParse(s: String, delimeter: String): Seq[String] =
  s.split(delimeter).map(_.trim).toSeq
