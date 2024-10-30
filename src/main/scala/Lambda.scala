// λ項とは…
enum LambdaTerm:
  // 変数をひとつ書き下したものは λ 項
  case VarReference(variable: Variable)
  // 適用。(λx. x x)(λy. y) を Application(λx. x x, λy. y) として扱いたい
  case Application(left: LambdaTerm, right: LambdaTerm)
  // λ抽象。λx. x x のことを Abstraction("x", Application(VarReference("x"), VarReference("x"))) として扱いたい
  case Abstraction(boundVar /* 束縛変数 */: Variable, body: LambdaTerm)

object LambdaTerm {
  extension (t: LambdaTerm)
    def freeVariables: Set[Variable] = t match
      case VarReference(v)             => Set(v)
      case Application(left, right)    => left.freeVariables ++ right.freeVariables
      case Abstraction(boundVar, body) => body.freeVariables - boundVar

    def variables: Set[Variable] = t match
      case VarReference(v)             => Set(v)
      case Application(left, right)    => left.variables ++ right.variables
      case Abstraction(boundVar, body) => body.variables + boundVar

    def replaceFreeVariableWithAnother(
        original: Variable,
        newVariable: Variable
    ): LambdaTerm = t match
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

    // 捕獲回避代入
    // t.substitute(v, s) というのは、直感的に言えば、t の中の自由な v を s に置き換えたもの
    def substitute(
        varToReplace: Variable,
        term: LambdaTerm
    ): LambdaTerm = t match
      case VarReference(variable) =>
        if variable == varToReplace then term else t
      case Application(left, right) =>
        Application(
          left.substitute(varToReplace, term),
          right.substitute(varToReplace, term)
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
      case VarReference(_)                     => true
      case Application(Abstraction(_, _), _)   => false
      case Application(VarReference(_), right) => right.isNormalForm
      case Application(left @ Application(_, _), right) =>
        left.isNormalForm && right.isNormalForm
      case Abstraction(_, body) => body.isNormalForm

  // β簡約
  def betaReduceOnce(left: Abstraction, right: LambdaTerm): LambdaTerm =
    left.body.substitute(left.boundVar, right)
}
