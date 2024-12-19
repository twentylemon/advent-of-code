package org.lemon.advent.lib.parse

private trait Sep:
  val delimeter: String
  def unapplySeq[T](str: String)(using unapply: String => Option[T]): Option[Seq[T]] =
    Some(str.split(delimeter).map(_.trim).toSeq.flatMap(unapply))

/** Matches chunks of a generic type separated by commas.
  */
object Csv extends Sep:
  val delimeter = ","

/** Matches chunks of a generic type separated by any whitespace.
  */
object Wsv extends Sep:
  val delimeter = "\\s+"

// givens for delimiter-separated values to be generic
given (String => Option[String]) = Some(_)

/** Matches chunks of text separated by two newlines.
  */
object Chunk:
  val delimeter = "\n\n"
  def unapplySeq(str: String): Option[Seq[String]] = Some(str.split(delimeter).map(_.trim).toSeq)
