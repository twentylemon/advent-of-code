package org.lemon.advent.lib

extension (str: String)
  /** Prepends the string with the given character until it reaches the given length.
    *
    * @param len the desired length of the string
    * @param c the character to prepend
    * @return the string padded to the left with the given character
    */
  def padLeft(len: Int, c: Char) = c.toString * (len - str.length) + str
