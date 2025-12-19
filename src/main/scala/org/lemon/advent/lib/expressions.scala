package org.lemon.advent.lib

import scala.collection.mutable

object ExpressionContext:
  def apply[T](expressions: Map[String, Expression[T]]): ExpressionContext[T] =
    ExpressionContext(expressions, mutable.Map.empty[String, Option[Option[T]]])

case class ExpressionContext[T](
    expressions: Map[String, Expression[T]],
    cache: mutable.Map[String, Option[Option[T]]],
):
  def resolve(name: String): Option[T] =
    cache.get(name) match
      case Some(Some(result)) => result
      case Some(None) => None
      case None =>
        cache(name) = None
        val result = expressions.get(name).flatMap(_.resolve(this))
        cache(name) = Some(result)
        result

  def simplify(name: String): Expression[T] = expressions(name).simplify(this)

trait Expression[T]:
  def resolve(context: ExpressionContext[T]): Option[T]
  def simplify(context: ExpressionContext[T]): Expression[T]

case class Literal[T](lit: T) extends Expression[T]:
  override def resolve(context: ExpressionContext[T]) = Some(lit)
  override def simplify(context: ExpressionContext[T]) = this

case class Reference[T](name: String) extends Expression[T]:
  override def resolve(context: ExpressionContext[T]) = context.resolve(name)
  override def simplify(context: ExpressionContext[T]) = context.simplify(name)

trait UnaryOperation[T] extends Expression[T]:
  def operand: Expression[T]
  def apply(operand: T): T
  def copy(operand: Expression[T]): UnaryOperation[T]

  override def resolve(context: ExpressionContext[T]) = for op <- operand.resolve(context) yield apply(op)

  override def simplify(context: ExpressionContext[T]) = resolve(context) match
    case Some(value) => Literal(value)
    case None => copy(operand = operand.simplify(context))

trait BinaryOperation[T] extends Expression[T]:
  def lhs: Expression[T]
  def rhs: Expression[T]
  def apply(left: T, right: T): T
  def copy(lhs: Expression[T], rhs: Expression[T]): BinaryOperation[T]

  override def resolve(context: ExpressionContext[T]) =
    for left <- lhs.resolve(context); right <- rhs.resolve(context) yield apply(left, right)

  override def simplify(context: ExpressionContext[T]) = resolve(context) match
    case Some(value) => Literal(value)
    case None => copy(lhs = lhs.simplify(context), rhs = rhs.simplify(context))

case class Unknown[T]() extends Expression[T]:
  override def resolve(context: ExpressionContext[T]) = None
  override def simplify(context: ExpressionContext[T]) = this
