package io.github.kory33.scala3_lambda_calculus.cek.extensions

import io.github.kory33.scala3_lambda_calculus.cek.EvaluatesTo
import io.github.kory33.scala3_lambda_calculus.cek.ValueClosure
import io.github.kory33.scala3_lambda_calculus.cek.EvaluationError.PrimitiveOperatorFailedToEvaluate
import io.github.kory33.scala3_lambda_calculus.cek.ExtendedLambdaTerm
import io.github.kory33.scala3_lambda_calculus.cek.ExtendedLambdaTerm.*

case class Natural(value: BigInt) {
  require(value >= 0)

  def +(that: Natural): Natural = Natural(value + that.value)
  def *(that: Natural): Natural = Natural(value * that.value)
}

type BoolOrNat = Boolean | Natural

enum ArithmeticOps {
  case Add, Mul, FirstLeqSecond, If
}

object ArithmeticOps {
  given EvaluatesTo[ArithmeticOps, BoolOrNat] with {
    override def eval(
        p: ArithmeticOps,
        args: List[ValueClosure[BoolOrNat, ArithmeticOps]]
    ): Either[
      PrimitiveOperatorFailedToEvaluate[BoolOrNat, ArithmeticOps],
      Value[BoolOrNat, ArithmeticOps]
    ] = p match {
      case Add =>
        args match {
          case List(ValueClosure(Constant(Natural(a)), _), ValueClosure(Constant(Natural(b)), _)) =>
            Right(Constant(Natural(a + b)))
          case List(ValueClosure(v1, _), ValueClosure(v2, _)) =>
            Left(PrimitiveOperatorFailedToEvaluate(s"Cannot add $v1 and $v2", p, args))
          case _ =>
            Left(PrimitiveOperatorFailedToEvaluate("Invalid number of arguments", p, args))
        }
      case Mul =>
        args match {
          case List(ValueClosure(Constant(Natural(a)), _), ValueClosure(Constant(Natural(b)), _)) =>
            Right(Constant(Natural(a * b)))
          case List(ValueClosure(v1, _), ValueClosure(v2, _)) =>
            Left(PrimitiveOperatorFailedToEvaluate(s"Cannot multiply $v1 and $v2", p, args))
          case _ =>
            Left(PrimitiveOperatorFailedToEvaluate("Invalid number of arguments", p, args))
        }
      case FirstLeqSecond =>
        args match {
          case List(ValueClosure(Constant(Natural(a)), _), ValueClosure(Constant(Natural(b)), _)) =>
            Right(Constant(a <= b))
          case List(ValueClosure(v1, _), ValueClosure(v2, _)) =>
            Left(PrimitiveOperatorFailedToEvaluate(s"Cannot compare $v1 and $v2", p, args))
          case _ =>
            Left(PrimitiveOperatorFailedToEvaluate("Invalid number of arguments", p, args))
        }
      case If =>
        args match {
          case List(ValueClosure(Constant(cond: Boolean), _), v1, v2) =>
            if cond then Right(v1.lambdaTerm) else Right(v2.lambdaTerm)
          case List(v1, _, _) =>
            Left(PrimitiveOperatorFailedToEvaluate(s"Expected a Boolean condition, found $v1", p, args))
          case _ =>
            Left(PrimitiveOperatorFailedToEvaluate("Invalid number of arguments", p, args))
        }
    }
  }
}