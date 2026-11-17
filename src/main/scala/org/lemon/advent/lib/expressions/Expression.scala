package org.lemon.advent.lib.expressions

import scala.collection.mutable

case class ExpressionContext[T](expressions: Map[String, Expression[T]]):
  private val resolved = mutable.Map.empty[String, Option[T]]
  private val resolving = mutable.Set.empty[String]
  private val simplifying = mutable.Set.empty[String]

  private def unlessInProgress[R](name: String, inProgress: mutable.Set[String], onCycle: => R)(body: => R): R =
    if inProgress(name) then onCycle
    else
      inProgress += name
      val result = body
      inProgress -= name
      result

  def resolve(name: String): Option[T] =
    resolved.getOrElseUpdate(
      name,
      unlessInProgress(name, resolving, None)(expressions.get(name).flatMap(_.resolve(this)))
    )

  def apply(name: String): T = resolve(name).getOrElse(throw new NoSuchElementException(s"$name does not resolve"))

  def simplify(name: String): Expression[T] = expressions.get(name) match
    case Some(expression) => unlessInProgress(name, simplifying, Reference(name))(expression.simplify(this))
    case None => Reference(name)

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
