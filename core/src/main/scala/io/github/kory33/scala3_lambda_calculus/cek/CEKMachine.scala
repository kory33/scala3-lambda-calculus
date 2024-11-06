package io.github.kory33.scala3_lambda_calculus.cek

import scala.annotation.tailrec
import io.github.kory33.scala3_lambda_calculus.foundation.{Variable, freshVariableNotIn}
import io.github.kory33.scala3_lambda_calculus.extended.ExtendedLambdaTerm
import cats.derived.*
import cats.Show
import cats.syntax.all.*
import io.github.kory33.scala3_lambda_calculus.extended.ExtendedLambdaTerm.Value

enum EvaluationError[C, P]:
  case VariableNotBound(variable: Variable)
  case ArgumentAppliedToConstant(constant: C, argument: ExtendedLambdaTerm.Value[C, P])
  case PrimitiveOperatorFailedToEvaluate(msg: String, operator: P, arguments: List[ValueClosure[C, P]])

case class ClosureWithTermRestriction[+C, +P, +TermSubClass <: ExtendedLambdaTerm[C, P]](
    lambdaTerm: TermSubClass,
    environment: Environment[C, P]
) {
  def supplementEnvironment[C2 >: C, P2 >: P](
      env: Environment[C2, P2]
  ): ClosureWithTermRestriction[C2, P2, TermSubClass] =
    ClosureWithTermRestriction(lambdaTerm, environment ++ env)
}
object ClosureWithTermRestriction {
  given [C: Show, P: Show, TermSubClass <: ExtendedLambdaTerm[C, P]]
      : Show[ClosureWithTermRestriction[C, P, TermSubClass]] with {
    def show(c: ClosureWithTermRestriction[C, P, TermSubClass]): String =
      s"Cl<${Show[ExtendedLambdaTerm[C, P]].show(c.lambdaTerm)}, ${c.environment.show}>"
  }
}

trait ClosureWithTermRestrictionSpecialization[TermSubClass[C, P] <: ExtendedLambdaTerm[C, P]] {
  def apply[C, P](
      lambdaTerm: TermSubClass[C, P],
      environment: Environment[C, P]
  ): ClosureWithTermRestriction[C, P, TermSubClass[C, P]] = ClosureWithTermRestriction(lambdaTerm, environment)
  def unapply[C, P](
      closure: ClosureWithTermRestriction[C, P, TermSubClass[C, P]]
  ): (TermSubClass[C, P], Environment[C, P]) = (closure.lambdaTerm, closure.environment)

  def withEmptyEnvironment[C, P](lambdaTerm: TermSubClass[C, P]): ClosureWithTermRestriction[C, P, TermSubClass[C, P]] =
    ClosureWithTermRestriction(lambdaTerm, Environment.empty)
}

type Closure[+C, +P] = ClosureWithTermRestriction[C, P, ExtendedLambdaTerm[C, P]]
object Closure extends ClosureWithTermRestrictionSpecialization[ExtendedLambdaTerm]
type ValueClosure[+C, +P] = ClosureWithTermRestriction[C, P, ExtendedLambdaTerm.Value[C, P]]
object ValueClosure extends ClosureWithTermRestrictionSpecialization[ExtendedLambdaTerm.Value]
type AbstractionClosure[+C, +P] = ClosureWithTermRestriction[C, P, ExtendedLambdaTerm.Abstraction[C, P]]
object AbstractionClosure extends ClosureWithTermRestrictionSpecialization[ExtendedLambdaTerm.Abstraction]

case class Environment[+C, +P](mapping: Map[Variable, Closure[C, P]]) {
  def overwrite[C2 >: C, P2 >: P](variable: Variable, closure: Closure[C2, P2]): Environment[C2, P2] =
    Environment(mapping + (variable -> closure))

  def lookup(variable: Variable): Option[Closure[C, P]] =
    mapping.get(variable)

  def ++[C2 >: C, P2 >: P](that: Environment[C2, P2]): Environment[C2, P2] =
    Environment(mapping ++ that.mapping)
}
object Environment {
  given [C: Show, P: Show]: Show[Environment[C, P]] with {
    def show(env: Environment[C, P]): String =
      "{" + env.mapping.map { case (k, v) => s"$k -> ${v.show}" }.mkString(", ") + "}"
  }

  def empty[C, P]: Environment[C, P] = Environment(Map.empty)
}

enum Continuation[C, P]:
  case ThenHalt[C, P]() extends Continuation[C, P]
  case ThenApplyAbstraction(
      abstraction: ClosureWithTermRestriction[C, P, ExtendedLambdaTerm.Abstraction[C, P]],
      andThen: Continuation[C, P]
  )
  case ThenEvalArg(
      arg: Closure[C, P],
      andThen: Continuation[C, P]
  )
  case ThenEvalOperatorArgs(
      operator: P,
      environmentAtOpApplication: Environment[C, P],
      alreadyEvaluatedReverseArgs: List[ValueClosure[C, P]],
      argsToEvaluate: List[ExtendedLambdaTerm[C, P]],
      andThen: Continuation[C, P]
  )

object Continuation {
  given [C: Show, P: Show]: Show[Continuation[C, P]] with {
    def show(k: Continuation[C, P]): String = {
      given Show[ExtendedLambdaTerm.Value[C, P]] = Show[ExtendedLambdaTerm[C, P]].narrow
      k match {
        case ThenHalt()                           => "ThenTerminate"
        case ThenApplyAbstraction(abstraction, k) => s"ThenApplyAbstraction(${abstraction.show}, ${k.show})"
        case ThenEvalArg(arg, k)                  => s"ThenEvalArg(${arg.show}, ${k.show})"
        case ThenEvalOperatorArgs(op, env, evaluated, toEval, k) =>
          val evaluatedStr = evaluated.map(_.show).mkString("[", ", ", "]")
          val toEvalStr = toEval.map(_.show).mkString("[", ", ", "]")
          s"ThenEvalOperatorArgs($op, $evaluatedStr, $toEvalStr, ${k.show})"
      }
    }
  }
}

object CEKMachineState {
  given [C: Show, P: Show]: Show[CEKMachineState[C, P]] with {
    def show(state: CEKMachineState[C, P]): String = state.showWithShowInstances
  }

  given Show[CEKMachineState[Nothing, Nothing]] with {
    def show(state: CEKMachineState[Nothing, Nothing]): String =
      state.showWithShowInstances(using Show[Unit].narrow, Show[Unit].narrow)
  }

  def stepContinuation[P, C](
      valueToPassToContinuation: ValueClosure[C, P],
      continuation: Continuation[C, P]
  )(using operatorEvaluator: P `EvaluatesTo` C): Either[EvaluationError[C, P], CEKMachineState[C, P]] = {
    import ExtendedLambdaTerm.*
    import Continuation.*

    (valueToPassToContinuation, continuation) match {
      case (v, ThenApplyAbstraction(AbstractionClosure(Abstraction(boundVar, body), env), k)) =>
        Right(CEKMachineState(Closure(body, env.overwrite(boundVar, v)), k))
      case (v, ThenEvalArg(closure, k)) =>
        v.lambdaTerm match {
          case abs: Abstraction[C, P] =>
            Right(CEKMachineState(closure, ThenApplyAbstraction(AbstractionClosure(abs, v.environment) /* == v*/, k)))
          case const: Constant[C, P] =>
            Left(EvaluationError.ArgumentAppliedToConstant(const.constant, v.lambdaTerm))
        }
      case (v, ThenEvalOperatorArgs(op, env, evaluated, nextArg :: rest, k)) =>
        Right(CEKMachineState(Closure(nextArg, env), ThenEvalOperatorArgs(op, env, v :: evaluated, rest, k)))
      case (v, ThenEvalOperatorArgs(op, env, evaluated, Nil, k)) =>
        operatorEvaluator.eval(op, (v :: evaluated).reverse).map { result =>
          CEKMachineState(result.supplementEnvironment(env), k)
        }
      case (v, ThenHalt()) =>
        Right(CEKMachineState(v, ThenHalt()))
    }
  }

  def stepClosureEval[P, C](
      closureToEvaluate: Closure[C, P],
      continuation: Continuation[C, P]
  )(using operatorEvaluator: P `EvaluatesTo` C): Either[EvaluationError[C, P], CEKMachineState[C, P]] = {
    import ExtendedLambdaTerm.*

    (closureToEvaluate, continuation) match {
      case (Closure(VarReference(v), env), k) =>
        env.lookup(v) match {
          case Some(cl) => Right(CEKMachineState(cl, k))
          case None     => Left(EvaluationError.VariableNotBound(v))
        }
      case (Closure(Application(m1, m2), env), k) =>
        Right(
          CEKMachineState(
            Closure(m1, env),
            Continuation.ThenEvalArg(Closure(m2, env), k)
          )
        )
      case (Closure(PrimitiveOperator(op, Nil), env), k) =>
        operatorEvaluator.eval(op, Nil).map { result =>
          CEKMachineState(result.supplementEnvironment(env), k)
        }
      case (Closure(PrimitiveOperator(op, m1 :: argsRest), env), k) =>
        Right(
          CEKMachineState(
            Closure(m1, env),
            Continuation.ThenEvalOperatorArgs(op, env, Nil, argsRest, k)
          )
        )
      case (Closure(value: (Abstraction[C, P] | Constant[C, P]), env), k) =>
        stepContinuation(ValueClosure(value, env), k)
    }
  }
}

trait EvaluatesTo[P, C] {
  def eval(p: P, args: List[ValueClosure[C, P]]): Either[
    EvaluationError.PrimitiveOperatorFailedToEvaluate[C, P],
    /* no free-variables */ Closure[C, P]
  ]
}

object EvaluatesTo {
  given EvaluatesTo[Nothing, Nothing] with {
    def eval(p: Nothing, args: List[ValueClosure[Nothing, Nothing]]): Either[
      EvaluationError.PrimitiveOperatorFailedToEvaluate[Nothing, Nothing],
      Closure[Nothing, Nothing]
    ] = p // unreachable
  }
}

case class CEKMachineState[C, P](
    closureToEvaluate: Closure[C, P],
    continuation: Continuation[C, P]
)(using operatorEvaluator: P `EvaluatesTo` C) {
  import CEKMachineState.*
  import ExtendedLambdaTerm.*

  def stepOnce: Either[EvaluationError[C, P], CEKMachineState[C, P]] =
    stepClosureEval(closureToEvaluate, continuation)

  def stepOnceOrThrow: CEKMachineState[C, P] =
    stepOnce match {
      case Left(e)  => throw new Exception(e.toString)
      case Right(s) => s
    }

  def isHaltingState: Boolean =
    continuation == Continuation.ThenHalt() && {
      closureToEvaluate.lambdaTerm match
        case _: Abstraction[C, P] => true
        case _: Constant[C, P]    => true
        case _                    => false
    }

  def showWithShowInstances(using Show[C], Show[P]): String =
    s"CEK(${closureToEvaluate.show}, ${continuation.show})"
}

class CEKMachine[C, P](private var state: CEKMachineState[C, P]) {
  def this(term: ExtendedLambdaTerm[C, P])(using operatorEvaluator: P `EvaluatesTo` C) =
    this(CEKMachineState(Closure.withEmptyEnvironment(term), Continuation.ThenHalt()))

  import CEKMachineState.*

  def runUntilTermination(stepLimit: Option[Int] = None): Either[EvaluationError[C, P], CEKMachineState[C, P]] = {
    var remainingSteps = stepLimit
    while remainingSteps.forall(_ > 0) && !state.isHaltingState do {
      state = state.stepOnce match {
        case Left(e)  => return Left(e)
        case Right(s) => s
      }
      remainingSteps = remainingSteps.map(_ - 1)
    }
    Right(state)
  }
}
