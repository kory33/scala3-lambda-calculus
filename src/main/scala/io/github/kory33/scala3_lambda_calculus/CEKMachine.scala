package io.github.kory33.scala3_lambda_calculus

import scala.annotation.tailrec

// 拡張λ項とは…
enum ExtendedLambdaTerm[ /* 定数の型 */ C, /* プリミティブオペレータの型 */ P]:
  // 変数をひとつ書き下したものは λ 項
  case VarReference(variable: Variable)
  // 適用。(λx. x x)(λy. y) を Application(λx. x x, λy. y) として扱いたい
  case Application(
      left: ExtendedLambdaTerm[C, P],
      right: ExtendedLambdaTerm[C, P]
  )
  // λ抽象。λx. x x のことを Abstraction("x", Application(VarReference("x"), VarReference("x"))) として扱いたい
  case Abstraction(
      boundVar /* 束縛変数 */: Variable,
      body: ExtendedLambdaTerm[C, P]
  )
  // 定数
  case Constant(constant: C)
  // プリミティブオペレータの適用
  case PrimitiveOperator(operator: P, arguments: List[ExtendedLambdaTerm[C, P]])

object ExtendedLambdaTerm {
  extension [C, P](t: ExtendedLambdaTerm[C, P])
    def freeVariables: Set[Variable] = t match
      case VarReference(v)             => Set(v)
      case Application(left, right)    => left.freeVariables ++ right.freeVariables
      case Abstraction(boundVar, body) => body.freeVariables - boundVar
      case Constant(_)                 => Set.empty
      case PrimitiveOperator(_, arguments) =>
        arguments.flatMap(_.freeVariables).toSet

    def variables: Set[Variable] = t match
      case VarReference(v)             => Set(v)
      case Application(left, right)    => left.variables ++ right.variables
      case Abstraction(boundVar, body) => body.variables + boundVar
      case Constant(_)                 => Set.empty
      case PrimitiveOperator(_, arguments) =>
        arguments.flatMap(_.variables).toSet

    def replaceFreeVariableWithAnother(
        original: Variable,
        newVariable: Variable
    ): ExtendedLambdaTerm[C, P] = t match
      case VarReference(v) =>
        if v == original then VarReference(newVariable) else VarReference(v)
      case Application(left, right) =>
        Application(
          left.replaceFreeVariableWithAnother(original, newVariable),
          right.replaceFreeVariableWithAnother(original, newVariable)
        )
      case Abstraction(boundVar, body) =>
        if boundVar == original then
          // original が束縛されちゃってたらその先は置き換えない
          Abstraction(boundVar, body)
        else
          Abstraction(
            boundVar,
            body.replaceFreeVariableWithAnother(original, newVariable)
          )
      case Constant(_) => t
      case PrimitiveOperator(operator, arguments) =>
        PrimitiveOperator(
          operator,
          arguments.map(_.replaceFreeVariableWithAnother(original, newVariable))
        )

    // 捕獲回避代入
    // t.substitute(v, s) というのは、直感的に言えば、t の中の自由な v を s に置き換えたもの
    def substitute(
        varToReplace: Variable,
        term: ExtendedLambdaTerm[C, P]
    ): ExtendedLambdaTerm[C, P] = t match
      case VarReference(variable) =>
        if variable == varToReplace then term else t
      case Application(left, right) =>
        Application(
          left.substitute(varToReplace, term),
          right.substitute(varToReplace, term)
        )
      case Constant(_) => t
      case PrimitiveOperator(operator, arguments) =>
        PrimitiveOperator(
          operator,
          arguments.map(_.substitute(varToReplace, term))
        )
      case Abstraction(boundVar, body) =>
        if (boundVar == varToReplace) {
          t
        } else if (!term.freeVariables.contains(boundVar)) {
          Abstraction(boundVar, body.substitute(varToReplace, term))
        } else {
          // 捕獲回避
          val newBoundVar = freshVariableNotIn(term.variables ++ body.variables)
          Abstraction(
            newBoundVar,
            body.replaceFreeVariableWithAnother(boundVar, newBoundVar)
          ).substitute(varToReplace, term)
        }

    // 正規形かどうか
    def isNormalForm: Boolean = t match
      case VarReference(_)                   => true
      case Application(Abstraction(_, _), _) => false
      case Application(left, right)          => left.isNormalForm && right.isNormalForm
      case Abstraction(_, body)              => body.isNormalForm
      case Constant(_)                       => true
      case PrimitiveOperator(_, arguments) =>
        arguments.forall(_.isNormalForm)

  // β簡約
  def betaReduceOnce[C, P](
      left: Abstraction[C, P],
      right: ExtendedLambdaTerm[C, P]
  ): ExtendedLambdaTerm[C, P] =
    left.body.substitute(left.boundVar, right)

  type Value[C, P] =
    ExtendedLambdaTerm.Abstraction[C, P] | ExtendedLambdaTerm.Constant[C, P]
}

enum EvaluationError[C, P]:
  case VariableNotBound(variable: Variable)
  case ArgumentAppliedToConstant(constant: C, argument: ExtendedLambdaTerm.Value[C, P])

object CEKMachineState {
  case class ClosureWithTermRestriction[C, P, +TermSubClass <: ExtendedLambdaTerm[C, P]](
      lambdaTerm: TermSubClass,
      environment: Environment[C, P]
  ) {
    require(lambdaTerm.freeVariables.subsetOf(environment.mapping.keySet))
  }
  trait ClosureWithTermRestrictionSpecialization[TermSubClass[C, P] <: ExtendedLambdaTerm[C, P]] {
    def apply[C, P](
        lambdaTerm: TermSubClass[C, P],
        environment: Environment[C, P]
    ): ClosureWithTermRestriction[C, P, TermSubClass[C, P]] = ClosureWithTermRestriction(lambdaTerm, environment)
    def unapply[C, P](
        closure: ClosureWithTermRestriction[C, P, TermSubClass[C, P]]
    ): (TermSubClass[C, P], Environment[C, P]) = (closure.lambdaTerm, closure.environment)
  }

  type Closure[C, P] = ClosureWithTermRestriction[C, P, ExtendedLambdaTerm[C, P]]
  object Closure extends ClosureWithTermRestrictionSpecialization[ExtendedLambdaTerm]
  type ValueClosure[C, P] = ClosureWithTermRestriction[C, P, ExtendedLambdaTerm.Value[C, P]]
  object ValueClosure extends ClosureWithTermRestrictionSpecialization[ExtendedLambdaTerm.Value]
  type AbstractionClosure[C, P] = ClosureWithTermRestriction[C, P, ExtendedLambdaTerm.Abstraction[C, P]]
  object AbstractionClosure extends ClosureWithTermRestrictionSpecialization[ExtendedLambdaTerm.Abstraction]

  case class Environment[C, P](mapping: Map[Variable, Closure[C, P]]) {
    def overwrite(variable: Variable, closure: Closure[C, P]): Environment[C, P] =
      Environment(mapping + (variable -> closure))

    def lookup(variable: Variable): Option[Closure[C, P]] =
      mapping.get(variable)
  }
  object Environment {
    def empty[C, P]: Environment[C, P] = Environment(Map.empty)
  }

  enum Continuation[C, P]:
    case ThenTerminate[C, P]() extends Continuation[C, P]
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
        alreadyEvaluatedReverseArgs: List[ValueClosure[C, P]],
        argsToEvaluate: List[Closure[C, P]],
        andThen: Continuation[C, P]
    )

  def stepContinuation[P, C](
      valueToPassToContinuation: ValueClosure[C, P],
      continuation: Continuation[C, P]
  )(using
      operatorEvaluator: P `EvaluatesTo` C
  ): Either[EvaluationError[C, P], CEKMachineState[C, P]] = {
    import ExtendedLambdaTerm.*
    import Continuation.*

    (valueToPassToContinuation, continuation) match {
      case (v, ThenApplyAbstraction(AbstractionClosure(Abstraction(boundVar, body), env), k)) =>
        Right(CEKMachineState(Closure(body, env.overwrite(boundVar, v)), k))
      case (v, ThenEvalArg(closure, k)) =>
        v.lambdaTerm match {
          case abs: Abstraction[C, P] =>
            Right(CEKMachineState(closure, ThenApplyAbstraction(AbstractionClosure(abs, v.environment), k)))
          case const: Constant[C, P] =>
            Left(EvaluationError.ArgumentAppliedToConstant(const.constant, v.lambdaTerm))
        }
      case (v, ThenEvalOperatorArgs(op, evaluated, nextArg :: rest, k)) =>
        Right(CEKMachineState(nextArg, ThenEvalOperatorArgs(op, v :: evaluated, rest, k)))
      case (v, ThenEvalOperatorArgs(op, evaluated, Nil, k)) =>
        Right(CEKMachineState(Closure(operatorEvaluator.eval(op, evaluated.reverse), Environment.empty), k))
      case (v, ThenTerminate()) =>
        Right(CEKMachineState(v, ThenTerminate()))
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
        Right(
          CEKMachineState(
            Closure(operatorEvaluator.eval(op, Nil), Environment.empty),
            k
          )
        )
      case (Closure(PrimitiveOperator(op, m1 :: argsRest), env), k) =>
        Right(
          CEKMachineState(
            Closure(m1, env),
            Continuation.ThenEvalOperatorArgs(op, Nil, argsRest.map(m => Closure(m, env)), k)
          )
        )
      case (Closure(abs @ Abstraction(_, _), env), k) =>
        stepContinuation(ValueClosure(abs, env), k)
      case (Closure(const @ Constant(_), env), k) =>
        stepContinuation(ValueClosure(const, env), k)
    }
  }
}

trait EvaluatesTo[P, C] {
  def eval(p: P, args: List[CEKMachineState.ValueClosure[C, P]]):
  /* no free-variables */ ExtendedLambdaTerm.Value[C, P]
}

case class CEKMachineState[C, P](
    closureToEvaluate: CEKMachineState.Closure[C, P],
    continuation: CEKMachineState.Continuation[C, P]
)(using operatorEvaluator: P `EvaluatesTo` C) {
  import CEKMachineState.*
  import ExtendedLambdaTerm.*

  def stepOnce: Either[EvaluationError[C, P], CEKMachineState[C, P]] =
    stepClosureEval(closureToEvaluate, continuation)

  def stepUntilTermination(stepLimit: Option[Int] = None): Either[EvaluationError[C, P], CEKMachineState[C, P]] =
    @tailrec def loop(
        state: CEKMachineState[C, P],
        stepLimit: Option[Int]
    ): Either[EvaluationError[C, P], CEKMachineState[C, P]] =
      if stepLimit.exists(_ <= 0) then Right(state)
      else if state.continuation == Continuation.ThenTerminate() then Right(state)
      else
        state.stepOnce match {
          case Left(e)  => Left(e)
          case Right(s) => loop(s, stepLimit.map(_ - 1))
        }

    loop(this, stepLimit)
}
