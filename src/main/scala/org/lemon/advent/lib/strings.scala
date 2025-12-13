package org.lemon.advent.lib

extension (str: String)
  /** Prepends the string with the given character until it reaches the given length.
    *
    * @param len the desired length of the string
    * @param c the character to prepend
    * @return the string padded to the left with the given character
    */
  def padLeft(len: Int, c: Char) = c.toString * (len - str.length) + str

  // TODO move parsing utilities here; remove parse entirely
  // wsv: Seq[String]
  // chunks: Seq[String]

  /** Splits the string by commas into a sequence of strings.
    *
    * @return the comma-separated values
    */
  def csv: Seq[String] = str.split(",").map(_.trim).filter(_.nonEmpty).toSeq
