package org.lemon.advent.lib.parse

/** Matches chunks of text separated by two newlines.
  */
object Chunk:
  val delimiter = "\n\n"
  def unapplySeq(str: String): Option[Seq[String]] = Some(str.split(delimiter).map(_.trim).toSeq)
