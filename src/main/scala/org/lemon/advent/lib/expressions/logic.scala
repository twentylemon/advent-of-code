package org.lemon.advent.lib.expressions

case class And(lhs: Expression[Boolean], rhs: Expression[Boolean]) extends BinaryOperation[Boolean]:
  override def apply(left: Boolean, right: Boolean) = left && right

case class Or(lhs: Expression[Boolean], rhs: Expression[Boolean]) extends BinaryOperation[Boolean]:
  override def apply(left: Boolean, right: Boolean) = left || right

case class Xor(lhs: Expression[Boolean], rhs: Expression[Boolean]) extends BinaryOperation[Boolean]:
  override def apply(left: Boolean, right: Boolean) = left ^ right

case class Not(operand: Expression[Boolean]) extends UnaryOperation[Boolean]:
  override def apply(operand: Boolean) = !operand
