package org.lemon.advent.lib.expressions

import scala.math.Integral.Implicits.*
import scala.math.Numeric.Implicits.*

case class Add[T: Numeric](lhs: Expression[T], rhs: Expression[T]) extends BinaryOperation[T]:
  override def apply(left: T, right: T) = left + right
  override def copy(lhs: Expression[T], rhs: Expression[T]) = Add(lhs, rhs)

case class Subtract[T: Numeric](lhs: Expression[T], rhs: Expression[T]) extends BinaryOperation[T]:
  override def apply(left: T, right: T) = left - right
  override def copy(lhs: Expression[T], rhs: Expression[T]) = Subtract(lhs, rhs)

case class Multiply[T: Numeric](lhs: Expression[T], rhs: Expression[T]) extends BinaryOperation[T]:
  override def apply(left: T, right: T) = left * right
  override def copy(lhs: Expression[T], rhs: Expression[T]) = Multiply(lhs, rhs)

case class Divide[T: Integral](lhs: Expression[T], rhs: Expression[T]) extends BinaryOperation[T]:
  override def apply(left: T, right: T) = left / right
  override def copy(lhs: Expression[T], rhs: Expression[T]) = Divide(lhs, rhs)

case class Negate[T: Numeric](operand: Expression[T]) extends UnaryOperation[T]:
  override def apply(operand: T) = -operand
  override def copy(operand: Expression[T]) = Negate(operand)
