package io.github.kory33.scala3_lambda_calculus.util.parsing

import scala.util.parsing.input.Reader
import scala.util.parsing.input.Position
import scala.util.parsing.input.NoPosition

class VectorReader[+T](underlying: Vector[T]) extends Reader[T] {
  override def first: T = underlying.head
  override def rest: Reader[T] = new VectorReader(underlying.tail)

  override def pos: Position = NoPosition
  override def atEnd: Boolean = underlying.isEmpty

  override def drop(n: Int): Reader[T] = new VectorReader(underlying.drop(n))
}
