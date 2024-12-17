package org.lemon.advent.lib

object Parse:
  object Num:
    def unapply[T: Numeric](s: String): Option[T] = summon[Numeric[T]].parseString(s)

  private def splitParse[T: Numeric](s: String, delimeter: String): Option[Seq[T]] =
    Some(s.split(delimeter).map(_.trim).toSeq.flatMap(Num.unapply[T]))

  object NumCsv:
    def unapply[T: Numeric](s: String): Option[Seq[T]] = splitParse(s, ",")

  object NumWsv:
    def unapply[T: Numeric](s: String): Option[Seq[T]] = splitParse(s, "\\s+")

  object Chunk:
    def unapply(s: String): Option[Seq[String]] = Some(s.split("\n\n").toSeq)

  object Int:
    def unapply(s: String): Option[Int] = Num.unapply(s)
  object IntCsv:
    def unapply(s: String): Option[Seq[Int]] = NumCsv.unapply[Int](s)
  object IntWsv:
    def unapply(s: String): Option[Seq[Int]] = NumWsv.unapply[Int](s)

  object Long:
    def unapply(s: String): Option[Long] = Num.unapply(s)
  object LongCsv:
    def unapply(s: String): Option[Seq[Long]] = NumCsv.unapply[Long](s)
  object LongWsv:
    def unapply(s: String): Option[Seq[Long]] = NumWsv.unapply[Long](s)
