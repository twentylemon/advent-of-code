package org.lemon.advent.year2015

import org.lemon.advent.lib.*

private object Day21:

  case class Item(cost: Int, damage: Int, armour: Int = 0)
  val weapons = Seq(Item(8, 4), Item(10, 5), Item(25, 6), Item(40, 7), Item(74, 8))
  val armour = Seq(Item(13, 0, 1), Item(31, 0, 2), Item(53, 0, 3), Item(75, 0, 4), Item(102, 0, 5))
  val rings = Seq(Item(25, 1), Item(50, 2), Item(100, 3), Item(20, 0, 1), Item(40, 0, 2), Item(80, 0, 3))

  case class Guy(hp: Int, damage: Int, armour: Int)

  def parse(input: String) = input.linesIterator.foldLeft(Guy(0, 0, 0))((boss, line) =>
    line match
      case s"Hit Points: $hp" => boss.copy(hp = hp.toInt)
      case s"Damage: $d" => boss.copy(damage = d.toInt)
      case s"Armor: $a" => boss.copy(armour = a.toInt)
  )

  def loadouts =
    for
      weapon <- weapons.iterator
      arm <- None +: armour.map(Some(_))
      ring1 <- None +: rings.map(Some(_))
      ring2 <- None +: rings.map(Some(_))
      if ring1.isEmpty || ring2.isEmpty || ring1 != ring2
    yield Seq(Some(weapon), arm, ring1, ring2).flatten

  def fight(me: Guy, boss: Guy) =
    boss.hp /^ (me.damage - boss.armour).max(1) <= me.hp /^ (boss.damage - me.armour).max(1)

  def part1(input: String) =
    val boss = parse(input)
    loadouts
      .filter(items => fight(Guy(hp = 100, damage = items.map(_.damage).sum, armour = items.map(_.armour).sum), boss))
      .map(_.map(_.cost).sum)
      .min

  def part2(input: String) =
    val boss = parse(input)
    loadouts
      .filterNot(items => fight(Guy(hp = 100, damage = items.map(_.damage).sum, armour = items.map(_.armour).sum), boss))
      .map(_.map(_.cost).sum)
      .max
