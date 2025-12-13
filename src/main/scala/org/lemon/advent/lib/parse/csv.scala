package org.lemon.advent.lib.parse

private trait GenSep:
  val delimiter: String
  def unapplySeq(str: String): Option[Seq[String]] =
    Some(str.split(delimiter).map(_.trim).toSeq.filter(_.nonEmpty))

/** Matches chunks separated by any whitespace.
  */
object Wsv extends GenSep:
  val delimiter = "\\s+"

/** Matches chunks of text separated by two newlines.
  */
object Chunk:
  val delimiter = "\n\n"
  def unapplySeq(str: String): Option[Seq[String]] = Some(str.split(delimiter).map(_.trim).toSeq)
