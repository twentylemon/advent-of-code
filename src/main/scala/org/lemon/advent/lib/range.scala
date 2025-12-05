package org.lemon.advent.lib

import scala.math.Ordering.Implicits._
import scala.collection.immutable.NumericRange

extension (range: Range)
  def intersects(rhs: Range): Boolean = range.min <= rhs.max && rhs.min <= range.max

  def intersect(rhs: Range): Range = math.max(range.min, rhs.min) to math.min(range.max, rhs.max)

  def union(rhs: Range): Seq[Range] =
    if range.intersects(rhs) then Seq(range.min.min(rhs.min) to range.max.max(rhs.max))
    else Seq(range, rhs)

  def diff(rhs: Range): Seq[Range] = ???

  def increasing: Range = if range.step > 0 then range else range.reverse
  def decreasing: Range = if range.step < 0 then range else range.reverse

  def asInclusive: Range = if range.isEmpty || range.isInclusive then range else range.min to range.max by range.step
  def asExclusive: Range = if range.isEmpty || !range.isInclusive then range else range.min to range.max by range.step

  def toNumericRange[N: Integral]: NumericRange[N] =
    val num = summon[Integral[N]]
    if range.isEmpty then NumericRange(num.zero, num.zero, num.one)
    else if range.isInclusive then
      NumericRange.inclusive(num.fromInt(range.start), num.fromInt(range.end), num.fromInt(range.step))
    else NumericRange.Exclusive(num.fromInt(range.start), num.fromInt(range.end), num.fromInt(range.step))

extension [N: Integral](range: NumericRange[N])
  def increasing: NumericRange[N] = if range.step > summon[Integral[N]].zero then range else range.reverse
  def decreasing: NumericRange[N] = if range.step < summon[Integral[N]].zero then range else range.reverse

  def asInclusive: NumericRange[N] =
    if range.isEmpty || range.isInclusive then range
    else NumericRange.inclusive(range.min, range.max, range.step)
  def asExclusive: NumericRange[N] =
    if range.isEmpty || !range.isInclusive then range
    else NumericRange.Exclusive(range.min, range.max, range.step)
