package org.lemon.advent.lib

import scala.math.Ordering.Implicits._
import scala.math.Integral.Implicits._
import scala.collection.immutable.{TreeMap, NumericRange}

extension (range: Range)
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

/** A discrete range of values between `start` and `end` inclusive.
  * Differs from a Range in that it is always inclusive and has a fixed step of 1.
  *
  * @param start the start of the interval (inclusive)
  * @param end the end of the interval (inclusive)
  * @tparam N the numeric type of values stored
  */
case class Interval[N: Integral](start: N, end: N) extends Iterable[N] with PartialFunction[N, N]:
  require(start <= end, s"Interval requires start <= end, got start=$start, end=$end")
  private val `1` = summon[Integral[N]].one

  def isInclusive: Boolean = true
  override def isEmpty: Boolean = false
  override def nonEmpty: Boolean = true
  override def size: Int = (end - start + `1`).toInt
  def length: N = end - start + `1`

  def contains(x: N): Boolean = x >= start && x <= end

  override def head: N = start
  override def headOption: Option[N] = Some(start)
  override def last: N = end
  override def lastOption: Option[N] = Some(end)
  override def min[B >: N](using Ordering[B]): N = start
  def minOption: Option[N] = Some(start)
  override def max[B >: N](using Ordering[B]): N = end
  def maxOption: Option[N] = Some(end)

  override def iterator: Iterator[N] = Iterator.iterate(start)(_ + `1`).takeWhile(_ <= end)

  override def apply(idx: N): N =
    val result = start + idx
    if !contains(result) then throw new IndexOutOfBoundsException(idx.toString)
    result

  def isDefinedAt(idx: N): Boolean = contains(start + idx)

  def intersects(rhs: Interval[N]): Boolean = start <= rhs.end && rhs.start <= end

  def toRange: Range = start.toInt to end.toInt
  def toNumericRange: NumericRange[N] = NumericRange.inclusive(start, end, `1`)

  override def toString: String = s"Interval($start..$end)"

object Interval:
  def apply(range: Range): Interval[Int] =
    require(range.step == 1, "Range must have step 1")
    Interval(range.min, range.max)

  def apply[N: Integral](range: NumericRange[N]): Interval[N] =
    require(range.step == 1, "Range must have step 1")
    Interval(range.min, range.max)

extension [N: Integral](tuple: (N, N))
  def asInterval: Interval[N] = Interval(tuple._1, tuple._2)

/** A Discrete Interval Encoding Tree for storing sets of discrete values by encoding contiguous intervals
  *  as single entries. When new ranges are added or removed, they are merged/split with existing intervals.
  *
  * @tparam N the numeric type of values stored
  * @param intervals the underlying interval storage, mapping each interval's start to its end (inclusive)
  */
case class Diet[N: Integral] private (intervals: TreeMap[N, N]):
  private val num = Integral[N]
  private val `1` = num.one

  /** Returns whether this tree contains the given value.
    *
    * @param value the value to check
    * @return true if the value is contained in any interval
    */
  def contains(value: N): Boolean = contains(value, value)

  /** Returns whether this tree contains an entire range.
    *
    * @param start the start of the range to check (inclusive)
    * @param end the end of the range to check (inclusive)
    * @return true if all values in [start, end] are contained in this diet
    */
  def contains(start: N, end: N): Boolean =
    if start > end then true
    else
      intervals.maxBefore(start + `1`) match
        case Some((s, e)) => start >= s && end <= e
        case None => false

  def contains(range: Range): Boolean = contains(range.toNumericRange)
  def contains(range: NumericRange[N]): Boolean =
    if range.isEmpty then true else contains(Interval(range.increasing.asInclusive))
  def contains(interval: Interval[N]): Boolean = contains(interval.start, interval.end)

  def apply(value: N): Boolean = contains(value)
  def apply(start: N, end: N): Boolean = contains(start, end)
  def apply(range: Range): Boolean = contains(range)
  def apply(range: NumericRange[N]): Boolean = contains(range)
  def apply(interval: Interval[N]): Boolean = contains(interval)

  def isEmpty: Boolean = intervals.isEmpty
  def nonEmpty: Boolean = intervals.nonEmpty

  /** @return the total count of discrete values in the tree
    */
  def size: N = intervals.map((start, end) => end - start + `1`).sum

  /** Adds a single value to the tree.
    *
    * @param value the value to add
    * @return a new diet containing the value
    */
  def add(value: N): Diet[N] = add(value, value)

  /** Adds an inclusive range to the tree.
    *
    * @param start the start of the range (inclusive)
    * @param end the end of the range (inclusive)
    * @return a new diet containing all values in the range
    */
  def add(start: N, end: N): Diet[N] =
    if start > end then this
    else
      // check if we can merge with an interval ending just before our start
      val (mergeStart, mergeEnd, baseIntervals) = intervals.maxBefore(start) match
        case Some((s, e)) if e >= start - `1` => (s, end max e, intervals.removed(s))
        case _ => (start, end, intervals)

      // find all intervals that our range covers or touches
      val absorbed = baseIntervals.rangeFrom(mergeStart).takeWhile((s, _) => s <= mergeEnd + `1`)
      if absorbed.isEmpty then Diet(baseIntervals.updated(mergeStart, mergeEnd))
      else
        val finalEnd = absorbed.last._2 max mergeEnd
        val finalIntervals = baseIntervals -- absorbed.keys
        Diet(finalIntervals.updated(mergeStart, finalEnd))

  /** Adds a range to this diet. The range is converted to increasing, so `5 to 3 by -1` will still add `3 to 5`.
    *
    * @param range the range to add
    * @return a new diet containing all values in the range
    * @throws IllegalArgumentException if the range has a step other than 1
    */
  def add(range: Range): Diet[N] = add(range.toNumericRange)
  def add(range: NumericRange[N]): Diet[N] = if range.isEmpty then this else add(Interval(range.increasing.asInclusive))
  def add(interval: Interval[N]): Diet[N] = add(interval.start, interval.end)

  def +(value: N): Diet[N] = add(value)
  def +(start: N, end: N): Diet[N] = add(start, end)
  def +(range: Range): Diet[N] = add(range)
  def +(range: NumericRange[N]): Diet[N] = add(range)
  def +(interval: Interval[N]): Diet[N] = add(interval)

  /** Removes a single value from the tree.
    *
    * @param value the value to remove
    * @return a new diet without the value
    */
  def remove(value: N): Diet[N] = remove(value, value)

  /** Removes an inclusive range from this diet.
    *
    * @param start the start of the range to remove (inclusive)
    * @param end the end of the range to remove (inclusive)
    * @return a new diet with the range removed
    */
  def remove(start: N, end: N): Diet[N] =
    if start > end then this
    else
      // handle interval that may overlap at the start
      val baseIntervals =
        intervals.maxBefore(start) match
          case Some((s, e)) if e >= start =>
            val removed = intervals.removed(s)
            val withLeft = if s < start then removed.updated(s, start - `1`) else removed
            if e > end then withLeft.updated(end + `1`, e) else withLeft
          case _ => intervals

      // handle intervals completely within the range and the one that may extend past end
      val affected = baseIntervals.rangeFrom(start).takeWhile((s, _) => s <= end)
      val finalIntervals = affected.foldLeft(baseIntervals) { case (acc, (s, e)) =>
        val removed = acc.removed(s)
        if e > end then removed.updated(end + `1`, e) else removed
      }

      Diet(finalIntervals)

  /** Removes a range to this diet. The range is converted to increasing, so `5 to 3 by -1` will still remove `3 to 5`.
    *
    * @param range the range to remove
    * @return a new diet with the range removed
    * @throws IllegalArgumentException if the range has a step other than 1
    */
  def remove(range: Range): Diet[N] = remove(range.toNumericRange)
  def remove(range: NumericRange[N]): Diet[N] =
    if range.isEmpty then this else remove(Interval(range.increasing.asInclusive))
  def remove(interval: Interval[N]): Diet[N] = remove(interval.start, interval.end)

  def -(value: N): Diet[N] = remove(value)
  def -(start: N, end: N): Diet[N] = remove(start, end)
  def -(range: Range): Diet[N] = remove(range)
  def -(range: NumericRange[N]): Diet[N] = remove(range)
  def -(interval: Interval[N]): Diet[N] = remove(interval)

  /** Returns the union of this diet and another.
    *
    * @param rhs the other diet
    * @return a new diet containing all values from both diets
    */
  def union(rhs: Diet[N]): Diet[N] =
    rhs.intervals.foldLeft(this)((diet, interval) => diet + (interval._1, interval._2))

  def ++(rhs: Diet[N]): Diet[N] = union(rhs)

  /** Returns the intersection of this diet and another.
    *
    * @param other the other diet
    * @return a new diet containing only values present in both diets
    */
  def intersect(rhs: Diet[N]): Diet[N] =
    def overlaps(start: N, end: N): Iterator[(N, N)] =
      // intervals from rhs that start within or adjacent to [start, end]
      val fromRange = rhs.intervals.rangeFrom(start - `1`).iterator.takeWhile((s, _) => s <= end + `1`)
      // interval that may start before but overlap
      val fromBefore = rhs.intervals.maxBefore(start).filter((_, e) => e >= start).iterator
      (fromRange ++ fromBefore).flatMap { (otherStart, otherEnd) =>
        val overlapStart = start max otherStart
        val overlapEnd = end min otherEnd
        Option.when(overlapStart <= overlapEnd)((overlapStart, overlapEnd))
      }

    Diet.fromIntervals(intervals.flatMap(overlaps))

  def &(rhs: Diet[N]): Diet[N] = intersect(rhs)

  /** Returns the difference of this diet and another (this - other).
    *
    * @param rhs the other diet
    * @return a new diet containing values in this diet but not in other
    */
  def diff(rhs: Diet[N]): Diet[N] =
    rhs.intervals.foldLeft(this)((diet, interval) => diet - (interval._1, interval._2))

  def --(rhs: Diet[N]): Diet[N] = diff(rhs)

  /** @return an iterator over all discrete values in this diet.
    */
  def iterator: Iterator[N] =
    intervals.iterator.flatMap((start, end) => NumericRange.inclusive(start, end, `1`).iterator)

  /** @return an iterator over all intervals in this diet as (start, end) pairs.
    */
  def intervalsIterator: Iterator[(N, N)] = intervals.iterator

  /** @return the intervals as a sequence of (start, end) pairs.
    */
  def toIntervals: Seq[(N, N)] = intervals.toSeq

  def toRanges: Seq[Range] = intervalsIterator.map(_.toInt to _.toInt).toSeq
  def toNumericRanges: Seq[NumericRange[N]] = intervalsIterator.map(NumericRange.inclusive(_, _, `1`)).toSeq

  def toSeq: Seq[N] = iterator.toSeq
  def toSet: Set[N] = iterator.toSet

  def min: N = intervals.head._1
  def minOption: Option[N] = intervals.headOption.map(_._1)
  def max: N = intervals.last._2
  def maxOption: Option[N] = intervals.lastOption.map(_._2)

  override def toString: String =
    if isEmpty then "Diet()"
    else intervals.map((s, e) => if s == e then s.toString else s"$s..$e").mkString("Diet(", ", ", ")")

object Diet:
  def empty[N: Integral]: Diet[N] = Diet(TreeMap.empty[N, N])

  def apply[N: Integral](values: N*): Diet[N] = apply(values)
  def apply[N: Integral](values: Iterable[N]): Diet[N] =
    values.iterator.foldLeft(empty[N])(_ + _)

  def apply[N: Integral](start: N, end: N): Diet[N] = empty[N].add(start, end)

  def apply(range: Range): Diet[Int] = empty[Int].add(range)
  def apply[N: Integral](range: NumericRange[N]): Diet[N] = empty[N].add(range)
  def apply[N: Integral](interval: Interval[N]): Diet[N] = empty[N].add(interval)

  def fromIntervals[N: Integral](intervals: Iterable[(N, N)]): Diet[N] =
    intervals.iterator.foldLeft(empty[N])((diet, interval) => diet.add(interval._1, interval._2))

  def fromRanges(ranges: Iterable[Range]): Diet[Int] = ranges.foldLeft(empty[Int])(_ + _)
  def fromRanges[N: Integral](ranges: Iterable[NumericRange[N]]): Diet[N] = ranges.foldLeft(empty[N])(_ + _)
