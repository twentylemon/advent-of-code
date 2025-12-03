package org.lemon.advent.lib.parse

/** Generic extractor object for parsing numbers.
  */
object Num:
  def unapply[T: Numeric](s: String): Option[T] = Numeric[T].parseString(s)
