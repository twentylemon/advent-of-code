package org.lemon.advent.lib.parse

private trait GenSep:
  val delimeter: String
  def unapplySeq(str: String): Option[Seq[String]] =
    Some(str.split(delimeter).map(_.trim).toSeq.filter(_.nonEmpty))

/** Matches chunks separated by commas.
  */
object Csv extends GenSep:
  val delimeter = ","

/** Matches chunks separated by any whitespace.
  */
object Wsv extends GenSep:
  val delimeter = "\\s+"

/** Matches chunks of text separated by two newlines.
  */
object Chunk:
  val delimeter = "\n\n"
  def unapplySeq(str: String): Option[Seq[String]] = Some(str.split(delimeter).map(_.trim).toSeq)
