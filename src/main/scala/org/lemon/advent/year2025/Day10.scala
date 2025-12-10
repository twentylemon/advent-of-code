package org.lemon.advent.year2025

import org.lemon.advent.lib.*

private object Day10:

  def parse(input: String) =
    import org.lemon.advent.lib.parse.*
    input.linesIterator.map(_ match
      case Wsv(s"[$target]", rest*) =>
        val buttons = rest.init.map(_ match
          case s"(${Csv(buttons*)})" => buttons.map(_.toInt)
        )
        val joltage = rest.last match
          case s"{${Csv(joltage*)}}" => joltage.map(_.toInt)

        val targets = target.map(c => if c == '.' then 0 else 1)
        (targets, buttons, joltage)
    ).toSeq

  def solve(buttons: Seq[Seq[Int]], targetValues: Seq[Int], useParity: Boolean) =
    val model = MipModel("day10")
    val maxPress = if useParity then 1 else targetValues.max
    val presses = buttons.indices.map(i =>
      if useParity then model.boolVar(s"press_$i") // each button is pressed 0 or 1 times
      else model.intVar(s"press_$i", 0, maxPress) // each button is pressed up to max(joltage) times
    )

    // for each light, the sum of button presses affecting it must have correct parity
    targetValues.indices.foreach { idx =>
      val affectingPresses = buttons.zip(presses).collect {
        case (toggles, press) if toggles.contains(idx) => LinearExpr(press)
      }
      val sumExpr = affectingPresses.reduce(_ + _)
      if useParity then
        // sum == target mod 2, ie sum = target + 2*parity for some parity >= 0
        val maxParity = (affectingPresses.size - targetValues(idx)) / 2
        val parity = model.intVar(s"parity_$idx", 0, maxParity)
        model.subjectTo(sumExpr === targetValues(idx) + 2 * parity)
      else
        model.subjectTo(sumExpr === targetValues(idx))
    }

    model.minimize(presses.foldLeft(LinearExpr.Zero)(_ + _))
    model.solve()
    val result = model.objective
    model.release()
    result.round.toInt

  def part1(input: String) =
    val problems = parse(input)
    problems.map((targets, buttons, _) => solve(buttons, targets, useParity = true)).sum

  def part2(input: String) =
    val problems = parse(input)
    problems.map((_, buttons, joltage) => solve(buttons, joltage, useParity = false)).sum
