package org.lemon.advent.lib

extension (str: String)
  def padLeft(n: Int, c: Char) = c.toString * (n - str.length) + str
