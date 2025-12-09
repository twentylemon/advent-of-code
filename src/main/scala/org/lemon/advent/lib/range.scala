package org.lemon.advent.lib

import scala.math.Ordering.Implicits.*
import scala.math.Integral.Implicits.*
import scala.collection.immutable.{TreeMap, NumericRange}

extension (range: Range)
  def toNumericRange[N: Integral]: NumericRange[N] =
    val num = summon[Integral[N]]
    if range.isEmpty then NumericRange(num.zero, num.zero, num.one)
    else if range.isInclusive then
      NumericRange.inclusive(num.fromInt(range.start), num.fromInt(range.end), num.fromInt(range.step))
    else NumericRange.Exclusive(num.fromInt(range.start), num.fromInt(range.end), num.fromInt(range.step))

  def toInterval: Interval[Int] = Interval(range)

extension [N: Integral](tuple: (N, N))
  def toInterval: Interval[N] = Interval(tuple._1, tuple._2)

/** A discrete range of values between `start` and `end` inclusive.
  * Differs from a Range in that it is always inclusive and has a fixed step of 1.
  * An interval is empty when `start > end`.
  *
  * @param start the start of the interval (inclusive)
  * @param end the end of the interval (inclusive)
  * @tparam N the numeric type of values stored
  */
case class Interval[N: Integral](start: N, end: N) extends Iterable[N] with PartialFunction[N, N]:
  private val num = summon[Integral[N]]
  private val `1` = num.one
  private val `0` = num.zero

  def isInclusive: Boolean = true
  override def isEmpty: Boolean = start > end
  override def nonEmpty: Boolean = start <= end

  @deprecated("use length instead")
  override def size: Int = length.toInt
  def length: N = if isEmpty then `0` else end - start + `1`

  def contains(x: N): Boolean = x >= start && x <= end
  def containsSlice(that: Interval[N]): Boolean = that.isEmpty || (nonEmpty && that.start >= start && that.end <= end)

  override def apply(idx: N): N =
    val result = start + idx
    if !contains(result) then throw new IndexOutOfBoundsException(idx.toString)
    result

  override def head: N = if isEmpty then throw new NoSuchElementException("head of empty interval") else start
  override def headOption: Option[N] = if isEmpty then None else Some(start)
  override def last: N = if isEmpty then throw new NoSuchElementException("last of empty interval") else end
  override def lastOption: Option[N] = if isEmpty then None else Some(end)

  def min: N = if isEmpty then throw new UnsupportedOperationException("min of empty interval") else start
  def minOption: Option[N] = if isEmpty then None else Some(start)
  def max: N = if isEmpty then throw new UnsupportedOperationException("max of empty interval") else end
  def maxOption: Option[N] = if isEmpty then None else Some(end)

  override def iterator: Iterator[N] =
    if isEmpty then Iterator.empty else Iterator.iterate(start)(_ + `1`).takeWhile(_ <= end)

  def reverseIterator: Iterator[N] =
    if isEmpty then Iterator.empty else Iterator.iterate(end)(_ - `1`).takeWhile(_ >= start)

  def indices: Interval[N] = if isEmpty then Interval.empty else Interval(`0`, length - `1`)

  def slice(from: N, until: N): Interval[N] =
    if isEmpty then this
    else
      val clampedFrom = (from max `0`) min length
      val clampedUntil = (until max `0`) min length
      if clampedFrom >= clampedUntil then Interval.empty
      else Interval(start + clampedFrom, start + clampedUntil - `1`)

  def take(n: N): Interval[N] = slice(`0`, n)
  def takeRight(n: N): Interval[N] = slice(length - n, length)
  def drop(n: N): Interval[N] = slice(n, length)
  def dropRight(n: N): Interval[N] = slice(`0`, length - n)

  def splitAt(n: N): (Interval[N], Interval[N]) = (take(n), drop(n))

  override def tail: Interval[N] =
    if isEmpty then throw new UnsupportedOperationException("tail of empty interval")
    else Interval(start + `1`, end)

  override def init: Interval[N] =
    if isEmpty then throw new UnsupportedOperationException("init of empty interval")
    else Interval(start, end - `1`)

  def indexOf(elem: N): N = if contains(elem) then elem - start else - `1`
  def lastIndexOf(elem: N): N = indexOf(elem)

  def isDefinedAt(idx: N): Boolean = contains(start + idx)

  def intersects(rhs: Interval[N]): Boolean = nonEmpty && rhs.nonEmpty && start <= rhs.end && rhs.start <= end

  def intersect(rhs: Interval[N]): Interval[N] =
    if !intersects(rhs) then Interval.empty else Interval(start max rhs.start, end min rhs.end)

  def &(rhs: Interval[N]): Interval[N] = intersect(rhs)

  def toRange: Range = if isEmpty then 0 until 0 else start.toInt to end.toInt
  def toNumericRange: NumericRange[N] =
    if isEmpty then NumericRange(`1`, `0`, `1`) else NumericRange.inclusive(start, end, `1`)

  override def toString: String = if isEmpty then "Interval.empty" else s"Interval($start..$end)"

object Interval:
  def empty[N: Integral]: Interval[N] = Interval(summon[Integral[N]].one, summon[Integral[N]].zero)

  def apply(range: Range): Interval[Int] = apply(range.toNumericRange[Int])

  def apply[N: Integral](range: NumericRange[N]): Interval[N] =
    require(range.step == 1 || range.step == -1, "Range must have step 1 or -1")
    if range.isEmpty then empty[N]
    else if range.step == 1 then Interval(range.start, range.end)
    else Interval(range.end, range.start)

  given [N: Integral]: Ordering[Interval[N]] = Ordering.by[Interval[N], N](_.start).orElseBy(_.end)

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
  def contains(range: NumericRange[N]): Boolean = contains(Interval(range))
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
  def add(range: NumericRange[N]): Diet[N] = add(Interval(range))
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
  def remove(range: NumericRange[N]): Diet[N] = remove(Interval(range))
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

  def toRanges: Seq[Range] = intervalsIterator.map(_.toInt to _.toInt).toSeq
  def toNumericRanges: Seq[NumericRange[N]] = intervalsIterator.map(NumericRange.inclusive(_, _, `1`)).toSeq
  def toIntervals: Seq[Interval[N]] = intervalsIterator.map(Interval(_, _)).toSeq

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
