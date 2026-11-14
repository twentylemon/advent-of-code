package org.lemon.advent.lib

import scala.math.Ordering.Implicits.*
import scala.math.Integral.Implicits.*
import scala.collection.AbstractIterator
import scala.collection.immutable.{TreeMap, NumericRange}
import scala.annotation.targetName

extension (range: Range)
  def toNumericRange[N: Integral]: NumericRange[N] =
    if range.isInclusive then NumericRange.inclusive(fromInt(range.start), fromInt(range.end), fromInt(range.step))
    else NumericRange.Exclusive(fromInt(range.start), fromInt(range.end), fromInt(range.step))

  def toInterval: Interval[Int] = Interval(range)

extension [N: Integral](range: NumericRange[N])
  def toInterval: Interval[N] = Interval(range)

extension [N: Integral](tuple: (N, N))
  def toInterval: Interval[N] = Interval(tuple._1, tuple._2)

object Interval:
  def empty[N: Integral]: Interval[N] = Interval(`1`, `0`)

  def apply[N](range: Range)(using DefaultsTo[N, Int], Integral[N]): Interval[N] =
    if range.isEmpty then empty
    else
      require(range.step == 1 || range.step == -1, "Range must have step 1 or -1")
      Interval[N](fromInt(range.min), fromInt(range.max))

  def apply[N: Integral](range: NumericRange[N]): Interval[N] =
    if range.isEmpty then empty
    else
      require(range.step == 1 || range.step == -1, "Range must have step 1 or -1")
      Interval(range.min, range.max)

  given [N: Integral]: Ordering[Interval[N]] = Ordering.by[Interval[N], N](_.start).orElseBy(_.end)

  given Conversion[Range, Interval[Int]] = _.toInterval
  given [N: Integral]: Conversion[NumericRange[N], Interval[N]] = _.toInterval
  given [N: Integral]: Conversion[(N, N), Interval[N]] = _.toInterval

/** A discrete range of values between `start` and `end` inclusive.
  * Differs from a Range in that it is always inclusive and has a fixed step of 1.
  * An interval is empty when `start > end`.
  *
  * @param start the start of the interval (inclusive)
  * @param end the end of the interval (inclusive)
  * @tparam N the numeric type of values stored
  */
case class Interval[N: Integral](start: N, end: N) extends Iterable[N] with PartialFunction[N, N]:
  def isInclusive: Boolean = true
  override def isEmpty: Boolean = start > end

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
  def minOption: Option[N] = headOption
  def max: N = if isEmpty then throw new UnsupportedOperationException("max of empty interval") else end
  def maxOption: Option[N] = lastOption

  override def iterator: Iterator[N] = steps(start, end, `1`)
  def reverseIterator: Iterator[N] = steps(end, start, `-1`)

  private def steps(first: N, stop: N, step: N): Iterator[N] =
    if isEmpty then Iterator.empty
    else
      new AbstractIterator[N]:
        private var current = first
        private var more = true
        def hasNext: Boolean = more
        def next(): N =
          if !more then Iterator.empty.next()
          val result = current
          if result == stop then more = false else current = result + step
          result

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
    else drop(`1`)

  override def init: Interval[N] =
    if isEmpty then throw new UnsupportedOperationException("init of empty interval")
    else dropRight(`1`)

  override def tails: Iterator[Interval[N]] = Interval(`0`, length).iterator.map(drop)
  override def inits: Iterator[Interval[N]] = Interval(`0`, length).iterator.map(dropRight)

  def indexOf(elem: N): N = if contains(elem) then elem - start else - `1`
  def lastIndexOf(elem: N): N = indexOf(elem)

  def isDefinedAt(idx: N): Boolean = contains(start + idx)

  def intersects(rhs: Interval[N]): Boolean = nonEmpty && rhs.nonEmpty && start <= rhs.end && rhs.start <= end

  def intersect(rhs: Interval[N]): Interval[N] =
    if !intersects(rhs) then Interval.empty else Interval(start max rhs.start, end min rhs.end)

  def &(rhs: Interval[N]): Interval[N] = intersect(rhs)

  def shift(n: N): Interval[N] = Interval(start + n, end + n)
  def growStart(n: N): Interval[N] = Interval(start - n, end)
  def growEnd(n: N): Interval[N] = Interval(start, end + n)
  def expand(n: N): Interval[N] = Interval(start - n, end + n)
  def contract(n: N): Interval[N] = expand(-n)

  /** @return the value in this interval closest to `value` */
  def clamp(value: N): N =
    if isEmpty then throw new UnsupportedOperationException("clamp of empty interval")
    else value max start min end

  /** @return where `value` lands when this interval repeats forever in both directions */
  def wrap(value: N): N =
    if isEmpty then throw new UnsupportedOperationException("wrap of empty interval")
    else ((value - start) +% length) + start

  def toRange: Range = if isEmpty then 0 until 0 else start.toInt to end.toInt
  def toNumericRange: NumericRange[N] = NumericRange.inclusive(start, end, `1`)

  override def toString: String = if isEmpty then "Interval.empty" else s"Interval($start..$end)"

object Diet:
  def empty[N: Integral]: Diet[N] = Diet(TreeMap.empty[N, N])

  def apply[N: Integral](values: N*): Diet[N] = apply(values)
  def apply[N: Integral](values: Iterable[N]): Diet[N] =
    values.iterator.foldLeft(empty[N])(_ + _)

  def apply[N: Integral](start: N, end: N): Diet[N] = empty[N].add(start, end)

  def apply(range: Range): Diet[Int] = empty[Int].add(range)
  def apply[N: Integral](range: NumericRange[N]): Diet[N] = empty[N].add(range)
  def apply[N: Integral](interval: Interval[N]): Diet[N] = empty[N].add(interval)

  @targetName("fromIntervals")
  def apply[N: Integral](intervals: Iterable[Interval[N]]): Diet[N] =
    intervals.iterator.foldLeft(empty[N])((diet, interval) => diet.add(interval._1, interval._2))

  @targetName("fromIntervalPairs")
  def apply[N: Integral](intervals: Iterable[(N, N)]): Diet[N] =
    intervals.iterator.foldLeft(empty[N])((diet, interval) => diet.add(interval._1, interval._2))

  @targetName("fromRanges")
  def apply(ranges: Iterable[Range]): Diet[Int] = ranges.foldLeft(empty[Int])(_ + _)

  @targetName("fromNumericRanges")
  def apply[N: Integral](ranges: Iterable[NumericRange[N]]): Diet[N] = ranges.foldLeft(empty[N])(_ + _)

/** A Discrete Interval Encoding Tree for storing sets of discrete values by encoding contiguous intervals
  *  as single entries. When new ranges are added or removed, they are merged/split with existing intervals.
  *
  * @tparam N the numeric type of values stored
  * @param intervals the underlying interval storage, mapping each interval's start to its end (inclusive)
  */
case class Diet[N: Integral] private (intervals: TreeMap[N, N]):
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
      // the interval starting at or before `start`; when `start + 1` overflows, that is the last interval
      val next = start + `1`
      val floor = if next > start then intervals.maxBefore(next) else intervals.lastOption
      floor.exists((s, e) => start >= s && end <= e)

  def contains(range: Range): Boolean = contains(Interval[N](range))
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
  def size: N = intervals.iterator.map((start, end) => end - start + `1`).sum

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
      // whether an interval starting at `s` overlaps or touches one ending at `e`, without overflowing `e + 1`
      def reaches(s: N, e: N) = s <= e || s - `1` == e

      // check if we can merge with an interval ending just before our start
      val (mergeStart, mergeEnd, baseIntervals) = intervals.maxBefore(start) match
        case Some((s, e)) if e >= start - `1` => (s, end max e, intervals.removed(s))
        case _ => (start, end, intervals)

      // find all intervals that our range covers or touches
      val iter = baseIntervals.iteratorFrom(mergeStart)
      iter.nextOption() match
        case None => Diet(baseIntervals.updated(mergeStart, mergeEnd))
        case Some((s, _)) if !reaches(s, mergeEnd) => Diet(baseIntervals.updated(mergeStart, mergeEnd))
        case Some((s, e)) =>
          // at least one overlap, collect the rest
          val rest = iter.takeWhile((s2, _) => reaches(s2, mergeEnd max e)).toList
          val absorbed = (s, e) :: rest
          val finalEnd = absorbed.last._2 max mergeEnd
          val finalIntervals = absorbed.foldLeft(baseIntervals)((m, kv) => m - kv._1)
          Diet(finalIntervals.updated(mergeStart, finalEnd))

  /** Adds a range to this diet. The range is converted to increasing, so `5 to 3 by -1` will still add `3 to 5`.
    *
    * @param range the range to add
    * @return a new diet containing all values in the range
    * @throws IllegalArgumentException if the range has a step other than 1
    */
  def add(range: Range): Diet[N] = add(Interval[N](range))
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
            val withLeft = intervals.updated(s, start - `1`)
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
  def remove(range: Range): Diet[N] = remove(Interval[N](range))
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
      // interval that may start before but overlap
      val fromBefore = rhs.intervals.maxBefore(start).filter((_, e) => e >= start).iterator
      // intervals from rhs that start within [start, end]
      val fromRange = rhs.intervals.iteratorFrom(start).takeWhile((s, _) => s <= end)
      (fromBefore ++ fromRange).map((otherStart, otherEnd) => (start max otherStart, end min otherEnd))

    Diet(intervals.flatMap(overlaps))

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
    intervals.iterator.flatMap((start, end) => Interval(start, end).iterator)

  /** @return an iterator over all intervals in this diet as (start, end) pairs.
    */
  def intervalsIterator: Iterator[Interval[N]] = intervals.iterator.map(_.toInterval)

  def toRanges: Seq[Range] = intervalsIterator.map(_.toRange).toSeq
  def toNumericRanges: Seq[NumericRange[N]] = intervalsIterator.map(_.toNumericRange).toSeq
  def toIntervals: Seq[Interval[N]] = intervalsIterator.toSeq

  def toSeq: Seq[N] = iterator.toSeq
  def toSet: Set[N] = iterator.toSet

  def min: N = intervals.head._1
  def minOption: Option[N] = intervals.headOption.map(_._1)
  def max: N = intervals.last._2
  def maxOption: Option[N] = intervals.lastOption.map(_._2)

  override def toString: String =
    intervals.map((s, e) => if s == e then s.toString else s"$s..$e").mkString("Diet(", ", ", ")")
