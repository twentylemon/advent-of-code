package org.lemon.advent.lib

import scala.collection.mutable

case class ExpressionContext[T](expressions: Map[String, Expression[T]]):
  private val cache = mutable.Map.empty[String, Option[Option[T]]]
  private val simplifying = mutable.Set.empty[String]

  private[lib] def resolve(name: String): Option[T] =
    cache.get(name) match
      case Some(Some(result)) => result
      case Some(None) => None
      case None =>
        cache(name) = None
        val result = expressions.get(name).flatMap(_.resolve(using this))
        cache(name) = Some(result)
        result

  private[lib] def simplify(name: String): Expression[T] = expressions.get(name) match
    case Some(expression) if !simplifying(name) =>
      simplifying += name
      val simplified = expression.simplify(using this)
      simplifying -= name
      simplified
    case _ => Reference(name)

trait Expression[T]:
  def resolve(using context: ExpressionContext[T]): Option[T]
  def simplify(using context: ExpressionContext[T]): Expression[T]

case class Literal[T](lit: T) extends Expression[T]:
  override def resolve(using context: ExpressionContext[T]) = Some(lit)
  override def simplify(using context: ExpressionContext[T]) = this

case class Reference[T](name: String) extends Expression[T]:
  override def resolve(using context: ExpressionContext[T]) = context.resolve(name)
  override def simplify(using context: ExpressionContext[T]) = context.simplify(name)

trait UnaryOperation[T] extends Expression[T]:
  def operand: Expression[T]
  def apply(operand: T): T
  def copy(operand: Expression[T]): UnaryOperation[T]

  override def resolve(using context: ExpressionContext[T]) = for op <- operand.resolve yield apply(op)

  override def simplify(using context: ExpressionContext[T]) = resolve match
    case Some(value) => Literal(value)
    case None => copy(operand = operand.simplify)

trait BinaryOperation[T] extends Expression[T]:
  def lhs: Expression[T]
  def rhs: Expression[T]
  def apply(left: T, right: T): T
  def copy(lhs: Expression[T], rhs: Expression[T]): BinaryOperation[T]

  override def resolve(using context: ExpressionContext[T]) =
    for left <- lhs.resolve; right <- rhs.resolve yield apply(left, right)

  override def simplify(using context: ExpressionContext[T]) = resolve match
    case Some(value) => Literal(value)
    case None => copy(lhs = lhs.simplify, rhs = rhs.simplify)
