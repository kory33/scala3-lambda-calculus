package io.github.kory33.scala3_lambda_calculus.extended.extensions

import io.github.kory33.scala3_lambda_calculus.cek.EvaluatesTo
import io.github.kory33.scala3_lambda_calculus.cek.ValueClosure
import io.github.kory33.scala3_lambda_calculus.cek.EvaluationError.PrimitiveOperatorFailedToEvaluate
import io.github.kory33.scala3_lambda_calculus.extended.ExtendedLambdaTerm
import io.github.kory33.scala3_lambda_calculus.extended.ExtendedLambdaTerm.*
import cats.Show
import cats.derived.*
import io.github.kory33.scala3_lambda_calculus.cek.Closure

case class Natural(value: BigInt) {
  require(value >= 0)

  def +(that: Natural): Natural = Natural(value + that.value)
  def *(that: Natural): Natural = Natural(value * that.value)
}

type BoolOrNat = Boolean | Natural
object BoolOrNat {
  given Show[BoolOrNat] with {
    override def show(t: BoolOrNat): String = t match {
      case b: Boolean => b.toString
      case Natural(n) => n.toString
    }
  }
}

enum ArithmeticOps derives Show {
  case Add, Mul, FirstLeqSecond, If, PredOrZero
}

object ArithmeticOps {
  given EvaluatesTo[ArithmeticOps, BoolOrNat] with {
    override def eval(
        p: ArithmeticOps,
        args: List[ValueClosure[BoolOrNat, ArithmeticOps]]
    ): Either[
      PrimitiveOperatorFailedToEvaluate[BoolOrNat, ArithmeticOps],
      Closure[BoolOrNat, ArithmeticOps]
    ] = p match {
      case Add =>
        args match {
          case List(ValueClosure(Constant(Natural(a)), _), ValueClosure(Constant(Natural(b)), _)) =>
            Right(Closure.withEmptyEnvironment(Constant(Natural(a + b))))
          case List(ValueClosure(v1, _), ValueClosure(v2, _)) =>
            Left(PrimitiveOperatorFailedToEvaluate(s"Cannot add $v1 and $v2", p, args))
          case _ =>
            Left(PrimitiveOperatorFailedToEvaluate("Invalid number of arguments", p, args))
        }
      case Mul =>
        args match {
          case List(ValueClosure(Constant(Natural(a)), _), ValueClosure(Constant(Natural(b)), _)) =>
            Right(Closure.withEmptyEnvironment(Constant(Natural(a * b))))
          case List(ValueClosure(v1, _), ValueClosure(v2, _)) =>
            Left(PrimitiveOperatorFailedToEvaluate(s"Cannot multiply $v1 and $v2", p, args))
          case _ =>
            Left(PrimitiveOperatorFailedToEvaluate("Invalid number of arguments", p, args))
        }
      case FirstLeqSecond =>
        args match {
          case List(ValueClosure(Constant(Natural(a)), _), ValueClosure(Constant(Natural(b)), _)) =>
            Right(Closure.withEmptyEnvironment(Constant(a <= b)))
          case List(ValueClosure(v1, _), ValueClosure(v2, _)) =>
            Left(PrimitiveOperatorFailedToEvaluate(s"Cannot compare $v1 and $v2", p, args))
          case _ =>
            Left(PrimitiveOperatorFailedToEvaluate("Invalid number of arguments", p, args))
        }
      case If =>
        args match {
          case List(ValueClosure(Constant(cond: Boolean), _), v1, v2) =>
            if cond then Right(v1) else Right(v2)
          case List(v1, _, _) =>
            Left(PrimitiveOperatorFailedToEvaluate(s"Expected a Boolean condition, found $v1", p, args))
          case _ =>
            Left(PrimitiveOperatorFailedToEvaluate("Invalid number of arguments", p, args))
        }
      case PredOrZero =>
        args match {
          case List(ValueClosure(Constant(Natural(n)), _)) =>
            Right(Closure.withEmptyEnvironment(Constant(Natural {
              if n == 0 then 0 else n - 1
            })))
          case List(ValueClosure(v, _)) =>
            Left(PrimitiveOperatorFailedToEvaluate(s"Expected a Natural number, found $v", p, args))
          case _ =>
            Left(PrimitiveOperatorFailedToEvaluate("Invalid number of arguments", p, args))
        }
    }
  }
}
