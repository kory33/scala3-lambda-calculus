package io.github.kory33.scala3_lambda_calculus.extended.parser

import io.github.kory33.scala3_lambda_calculus.extended.extensions.BoolOrNat
import io.github.kory33.scala3_lambda_calculus.extended.extensions.ArithmeticOps
import scala.util.parsing.input.Positional
import io.github.kory33.scala3_lambda_calculus.extended.extensions.Natural

object ArithmeticTokenizer extends ExtendedLambdaTermTokenizer[BoolOrNat, ArithmeticOps] {

  override def extensionTokenizer: Parser[Either[BoolOrNat, ArithmeticOps]] =
    "Add" ^^ { _ => Right(ArithmeticOps.Add) } |
      "Mul" ^^ { _ => Right(ArithmeticOps.Mul) } |
      "FirstLeqSecond" ^^ { _ => Right(ArithmeticOps.FirstLeqSecond) } |
      "If" ^^ { _ => Right(ArithmeticOps.If) } |
      """\d+""".r ^^ { num => Left(Natural(BigInt(num))) } |
      "true" ^^ { _ => Left(true) } |
      "false" ^^ { _ => Left(false) }

  def tokenize(input: String): Either[String, Vector[Token[BoolOrNat, ArithmeticOps]]] =
    parse(tokenStreamParser, input) match {
      case Success(tokens, _) => Right(tokens)
      case NoSuccess(msg, _)  => Left(msg)
      case Failure(msg, _)    => Left(msg)
      case Error(msg, _)      => Left(msg)
    }
}
