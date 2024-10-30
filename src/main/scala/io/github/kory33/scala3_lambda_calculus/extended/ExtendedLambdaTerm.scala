package io.github.kory33.scala3_lambda_calculus.extended

import io.github.kory33.scala3_lambda_calculus.foundation.Variable
import io.github.kory33.scala3_lambda_calculus.foundation.freshVariableNotIn
import cats.Show

// 拡張λ項とは…
enum ExtendedLambdaTerm[ /* 定数の型 */ +C, /* プリミティブオペレータの型 */ +P]:
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
  // プリミティブオペレータの適用 (e.g. "[Sum (λx. x) 3 5]")
  case PrimitiveOperator(operator: P, arguments: List[ExtendedLambdaTerm[C, P]])

object ExtendedLambdaTerm {
  given Show[ExtendedLambdaTerm[Nothing, Nothing]] with
    def show(t: ExtendedLambdaTerm[Nothing, Nothing]): String = ShowExtendedLambdaTerm.show(t)

  given [C: Show, P: Show]: Show[ExtendedLambdaTerm[C, P]] with
    def show(t: ExtendedLambdaTerm[C, P]): String = ShowExtendedLambdaTerm.show(t)

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
