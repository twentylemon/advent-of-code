package org.lemon.advent.lib

import scala.util.Try

extension (str: String)
  /** Prepends the string with the given character until it reaches the given length.
    *
    * @param len the desired length of the string
    * @param c the character to prepend
    * @return the string padded to the left with the given character
    */
  def padLeft(len: Int, c: Char) = c.toString * (len - str.length) + str

  /** Splits the string by commas into a sequence of strings.
    *
    * @return the comma-separated values
    */
  def csv: Seq[String] = str.split(",").map(_.trim).filter(_.nonEmpty).toSeq

  /** Splits the string by whitespace into a sequence of strings.
    *
    * @return the whitespace-separated values
    */
  def wsv: Seq[String] = str.split("\\s+").map(_.trim).filter(_.nonEmpty).toSeq

  /** Splits the string by double newlines into a sequence of strings.
    *
    * @return the chunks of text separated by blank lines
    */
  def chunks: Seq[String] = str.split("\n\n").filter(_.trim.nonEmpty).toSeq

  /** Parses the string into a BigInt.
    *
    * @return the string as a BigInt
    */
  def toBigInt: BigInt = BigInt(str)

  /** Tris to parse the string into a BigInt, returning None if it fails.
    *
    * @return the string as a BigInt, or None on failure
    */
  def toBigIntOption: Option[BigInt] = Try(BigInt(str)).toOption
