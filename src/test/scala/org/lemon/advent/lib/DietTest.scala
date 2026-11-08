package org.lemon.advent.lib

import org.lemon.advent.*
import org.scalacheck.{Gen, Prop}
import org.scalacheck.Prop.*
import org.scalacheck.commands.Commands

import scala.util.Try

class DietTest extends UnitTest:
  test("min of an empty diet throws NoSuchElementException") {
    a[NoSuchElementException] should be thrownBy Diet.empty[Int].min
  }

  test("max of an empty diet throws NoSuchElementException") {
    a[NoSuchElementException] should be thrownBy Diet.empty[Int].max
  }

  test("toString of an empty diet is Diet()") {
    Diet.empty[Int].toString shouldBe "Diet()"
  }

  test("toString formats a single value") {
    check((n: Int) => Diet(n).toString == s"Diet($n)")
  }

  test("toString formats an interval") {
    check((a: Int, b: Int) => a != b ==> (Diet(a min b, a max b).toString == s"Diet(${a min b}..${a max b})"))
  }

  test("toString formats separate values") {
    val separateValues =
      for
        count <- Gen.choose(2, 10)
        first <- Gen.choose(-1000, 1000)
        gaps <- Gen.listOfN(count - 1, Gen.choose(2, 100))
      yield gaps.scanLeft(first)(_ + _)
    check(forAll(separateValues) { values => Diet(values).toString == values.sorted.mkString("Diet(", ", ", ")") })
  }

  test("toString formats separate intervals") {
    val separateRanges =
      for
        count <- Gen.choose(2, 5)
        first <- Gen.choose(-1000, 1000)
        specs <- Gen.listOfN(count, for len <- Gen.choose(1, 50); gap <- Gen.choose(2, 100) yield (len, gap))
      yield specs.scanLeft((first, first)) { case ((_, prevEnd), (len, gap)) =>
        val start = prevEnd + gap
        (start, start + len - 1)
      }.tail.map((s, e) => s to e)
    check(forAll(separateRanges) { ranges =>
      Diet(ranges).toString == ranges
        .map(r => if r.size == 1 then r.min.toString else s"${r.min}..${r.max}")
        .mkString("Diet(", ", ", ")")
    })
  }

  def isCanonical(diet: Diet[Int]): Boolean =
    val intervals = diet.toIntervals
    intervals.forall(_.nonEmpty) && intervals.sliding2.forall((a, b) => a.end.toLong + 1 < b.start.toLong)

  class DietCommands(blockSize: Int, block: Gen[Int]) extends Commands:
    type State = Set[Int]
    final class Sut(var diet: Diet[Int])

    def first(b: Int) = (b.toLong * blockSize).toInt
    def last(b: Int) = ((b + 1L) * blockSize - 1).toInt
    def values(blocks: Range) =
      if blocks.isEmpty then blocks
      else if blocks.step > 0 then first(blocks.min) to last(blocks.max)
      else last(blocks.max) to first(blocks.min) by -1
    def runs(state: State): Seq[(Int, Int)] =
      state.toSeq.sorted
        .foldLeft(List.empty[(Int, Int)]) {
          case ((s, e) :: rest, b) if b == e + 1 => (s, b) :: rest
          case (acc, b) => (b, b) :: acc
        }
        .reverse
        .map((s, e) => (first(s), last(e)))

    val blocks =
      for
        from <- block
        length <- Gen.choose(0, 20)
        ascending = from to (from.toLong + length).min(Int.MaxValue / blockSize).toInt
        blocks <- Gen.oneOf(ascending, ascending.reverse, from until from)
      yield blocks
    val value = Gen.oneOf(block.map(first), block.map(last), block.flatMap(b => Gen.choose(first(b), last(b))))
    val valueRange =
      for
        a <- value
        b <- value
        range <- Gen.oneOf((a min b) to (a max b), (a max b) to (a min b) by -1, a until a)
      yield range

    def canCreateNewSut(newState: State, initSuts: Iterable[State], runningSuts: Iterable[Sut]) = true
    def newSut(state: State) =
      Sut(if blockSize == 1 then Diet(state) else Diet(state.toList.map(b => values(b to b))))
    def destroySut(sut: Sut) = ()
    def initialPreCondition(state: State) = true
    def genInitialState = Gen.containerOf[Set, Int](block)
    def genCommand(state: State) =
      val commands = Seq(
        blocks.map(Add(_)),
        blocks.map(Remove(_)),
        Gen.listOf(blocks).map(Union(_)),
        Gen.listOf(blocks).map(Intersect(_)),
        Gen.listOf(blocks).map(Diff(_)),
        value.map(Contains(_)),
        valueRange.map(ContainsRange(_)),
        Gen.const(IsEmpty),
        Gen.const(NonEmpty),
        Gen.const(Min),
        Gen.const(MinOption),
        Gen.const(Max),
        Gen.const(MaxOption),
        Gen.const(IntervalsIterator),
        Gen.const(ToRanges),
        Gen.const(ToNumericRanges),
      )
      val unitOnly = Seq(block.map(AddValue(_)), block.map(RemoveValue(_)), Gen.const(ToSeq), Gen.const(Size))
      Gen.oneOf(if blockSize == 1 then commands ++ unitOnly else commands).flatMap(identity)

    abstract class Update(f: Diet[Int] => Diet[Int], model: State => State) extends SuccessCommand:
      type Result = Diet[Int]
      def run(sut: Sut) =
        sut.diet = f(sut.diet)
        sut.diet
      def nextState(state: State) = model(state)
      def preCondition(state: State) = true
      def postCondition(state: State, diet: Diet[Int]): Prop =
        isCanonical(diet) && diet.toIntervals.map(i => (i.start, i.end)) == runs(model(state))

    case class AddValue(n: Int) extends Update(_ + n, _ + n)
    case class Add(blocks: Range) extends Update(_ + values(blocks), _ ++ blocks)
    case class RemoveValue(n: Int) extends Update(_ - n, _ - n)
    case class Remove(blocks: Range) extends Update(_ - values(blocks), _ -- blocks)
    case class Union(rhs: List[Range]) extends Update(_ ++ Diet(rhs.map(values)), _ ++ rhs.flatten)
    case class Intersect(rhs: List[Range]) extends Update(_ & Diet(rhs.map(values)), _ & rhs.flatten.toSet)
    case class Diff(rhs: List[Range]) extends Update(_ -- Diet(rhs.map(values)), _ -- rhs.flatten)

    abstract class Query[R](f: Diet[Int] => R, model: State => R) extends Command:
      type Result = R
      def run(sut: Sut) = f(sut.diet)
      def nextState(state: State) = state
      def preCondition(state: State) = true
      def postCondition(state: State, result: Try[R]): Prop = result.toOption == Try(model(state)).toOption

    case class Contains(n: Int) extends Query(_.contains(n), _.contains(Math.floorDiv(n, blockSize)))
    case class ContainsRange(range: Range)
        extends Query(
          _.contains(range),
          state =>
            range.isEmpty || (Math.floorDiv(range.min, blockSize) to Math.floorDiv(range.max, blockSize)).forall(state),
        )
    case object IsEmpty extends Query(_.isEmpty, _.isEmpty)
    case object NonEmpty extends Query(_.nonEmpty, _.nonEmpty)
    case object Min extends Query(_.min, state => first(state.min))
    case object MinOption extends Query(_.minOption, _.minOption.map(first))
    case object Max extends Query(_.max, state => last(state.max))
    case object MaxOption extends Query(_.maxOption, _.maxOption.map(last))
    case object IntervalsIterator extends Query(_.intervalsIterator.map(i => (i.start, i.end)).toSeq, runs)
    case object ToRanges extends Query(_.toRanges.map(r => (r.start, r.end)), runs)
    case object ToNumericRanges extends Query(_.toNumericRanges.map(r => (r.start, r.end)), runs)
    case object ToSeq extends Query(_.toSeq, _.toSeq.sorted)
    case object Size extends Query(_.size, _.size)

  test("operations match a Set and keep the diet canonical") {
    val nearZeroAndTheLimits = Gen.oneOf(
      Gen.choose(-50, 50),
      Gen.choose(Int.MinValue, Int.MinValue + 50),
      Gen.choose(Int.MaxValue - 50, Int.MaxValue),
    )
    check(DietCommands(1, nearZeroAndTheLimits).property())
  }

  test("operations on huge diets match a Set of blocks and keep the diet canonical") {
    check(DietCommands(1 << 24, Gen.chooseNum(-128, 127)).property())
  }
