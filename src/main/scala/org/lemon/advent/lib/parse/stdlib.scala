package org.lemon.advent.lib.parse

extension (int: Int.type)
  def unapply(str: String): Option[Int] = Num.unapply(str)

extension (long: Long.type)
  def unapply(str: String): Option[Long] = Num.unapply(str)

extension (bigint: BigInt.type)
  def unapply(str: String): Option[BigInt] = Num.unapply(str)

extension (range: Range.type)
  def unapply(str: String): Option[Range] =
    str match
      case s"${Int(start)}-${Int(end)}" => Some(start to end)
      case s"${Int(start)}..${Int(end)}" => Some(start to end)
      case _ => None

// givens for delimiter-separated values to be generic
given (String => Option[Int]) = Int.unapply
given (String => Option[Long]) = Long.unapply
given (String => Option[BigInt]) = BigInt.unapply
given (String => Option[Range]) = Range.unapply
