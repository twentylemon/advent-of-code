package org.lemon.advent.year2015

import org.lemon.advent.lib.*
import org.lemon.advent.lib.graph.*

private object Day22:

  case class State(hp: Int, mana: Int, bossHp: Int, shield: Int = 0, poison: Int = 0, recharge: Int = 0, dying: Int = 0)

  case class Spell(manaCost: Int, effect: State => State, canCastAt: State => Boolean = _ => true)
  val spells = Seq(
    Spell(53, s => s.copy(bossHp = s.bossHp - 4)),
    Spell(73, s => s.copy(hp = s.hp + 2, bossHp = s.bossHp - 2)),
    Spell(113, _.copy(shield = 6), _.shield <= 0),
    Spell(173, _.copy(poison = 6), _.poison <= 0),
    Spell(229, _.copy(recharge = 5), _.recharge <= 0),
  )

  case class Guy(hp: Int, damage: Int)

  def parse(input: String) = input.linesIterator.foldLeft(Guy(0, 0))((boss, line) =>
    line match
      case s"Hit Points: $hp" => boss.copy(hp = hp.toInt)
      case s"Damage: $d" => boss.copy(damage = d.toInt)
  )

  def effects(state: State) = state.copy(
    mana = state.mana + (if state.recharge > 0 then 101 else 0),
    bossHp = state.bossHp - (if state.poison > 0 then 3 else 0),
    shield = state.shield - 1,
    poison = state.poison - 1,
    recharge = state.recharge - 1,
  )

  def adjacency(boss: Guy)(state: State) =
    val adobe = effects(state).copy(hp = state.hp - state.dying)
    if adobe.hp <= 0 then Seq.empty
    else if adobe.bossHp <= 0 then Seq(adobe -> 0)
    else
      for
        spell <- spells
        if spell.manaCost <= adobe.mana
        if spell.canCastAt(adobe)
        afterCast = spell.effect(adobe).copy(mana = adobe.mana - spell.manaCost)
        bossEffects = effects(afterCast)
        afterBoss =
          if bossEffects.bossHp <= 0 then bossEffects
          else bossEffects.copy(hp = bossEffects.hp - (boss.damage - (if bossEffects.shield > 0 then 7 else 0)).max(1))
        if afterBoss.bossHp <= 0 || afterBoss.hp > 0
      yield (afterBoss, spell.manaCost)

  def part1(input: String) =
    val boss = parse(input)
    val start = State(hp = 50, mana = 500, bossHp = boss.hp)
    pathFind(adjacency(boss), start, _.bossHp <= 0).get.distance

  def part2(input: String) =
    val boss = parse(input)
    val start = State(hp = 50, mana = 500, bossHp = boss.hp, dying = 1)
    pathFind(adjacency(boss), start, _.bossHp <= 0).get.distance
