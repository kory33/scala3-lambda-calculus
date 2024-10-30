package io.github.kory33.scala3_lambda_calculus

// 変数。
// 実際のところ、Variable = AnyRef にしていい (値同士が区別可能で無限個あればなんでもいい) んですが、pretty-print しにくいのと、AnyRef をいっぱい作っておくのがひたすらに面倒なので、String を使います
// ちなみに、Variable = BigInteger でももちろんいいです
type Variable = String

def freshVariableNotIn(variableSet: /* 有限 */ Set[Variable]): Variable = {
  val alphabet = "abcdefghijklmnopqrstuvwxyz"
  val alphabetSize = alphabet.length

  def intToVariableName(n: Int): String = {
    if (n <= alphabetSize) alphabet.charAt(n - 1).toString
    else {
      val res = n % alphabetSize
      val div = n / alphabetSize
      if (res == 0) intToVariableName(div - 1) + alphabet(alphabetSize - 1)
      else intToVariableName(div) + alphabet(res - 1)
    }
  }

  @annotation.tailrec
  def loop(n: Int): Variable = {
    val candidate = intToVariableName(n)
    if variableSet(candidate) then loop(n + 1)
    else candidate
  }

  loop(1)
}
