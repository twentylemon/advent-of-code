package org.lemon.advent.lib

extension (range: Range)
  def intersects(rhs: Range): Boolean = range.min <= rhs.max && rhs.min <= range.max

  def intersect(rhs: Range): Range = math.max(range.min, rhs.min) to math.min(range.max, rhs.max)

  def union(rhs: Range): Seq[Range] =
    if range.intersects(rhs) then Seq(range.min.min(rhs.min) to range.max.max(rhs.max))
    else Seq(range, rhs)

  def diff(rhs: Range): Seq[Range] = ???

implicit object ParseRange:
  extension (range: Range.type)
    def unapply(str: String): Option[Range] =
      str match
        case s"${Int(start)}-${Int(end)}" => Some(start to end)
        case s"${Int(start)}..${Int(end)}" => Some(start to end)
        case _ => None
