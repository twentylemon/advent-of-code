package org.lemon.advent.lib

extension (str: String)
  /** Prepends `c` until the string is `len` long. A string that's already long enough is returned unchanged. */
  def padLeft(len: Int, c: Char): String = if len <= str.length then str else c.toString * (len - str.length) + str

  /** Splits on commas, trimming each value and dropping the blank ones. */
  def csv: Seq[String] = str.split(",").map(_.trim).filter(_.nonEmpty).toSeq

  /** Splits on runs of whitespace, ignoring any at either end. */
  def wsv: Seq[String] = str.split("\\s+").map(_.trim).filter(_.nonEmpty).toSeq

  /** Splits into the blocks of lines between empty lines. */
  def chunks: Seq[String] = str.split("\n\n").filter(_.trim.nonEmpty).toSeq

  /** Parses the string as a `BigInt`, ignoring surrounding whitespace. */
  def toBigInt: BigInt = BigInt(str.trim)
