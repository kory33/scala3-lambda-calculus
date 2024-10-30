package io.github.kory33.scala3_lambda_calculus.cek.parser

import io.github.kory33.scala3_lambda_calculus.foundation.Variable
import scala.util.parsing.combinator.RegexParsers

/** Tokens:
  *   - <LParen> ::= '('
  *   - <RParen> ::= ')'
  *   - <ArgBodySeparator> ::= '.'
  *   - <Variable> ::= [a-zA-Z_][a-zA-Z0-9_]*
  *   - <Lambda> ::= '\' | 'λ'
  *   - <Constant> ::= { implementation-defined }
  *   - <PrimitiveOperator> ::= { implementation-defined }
  */
enum Token[C, P]:
  case Constant(constant: C) extends Token[C, P]
  case PrimitiveOperator(operator: P) extends Token[C, P]
  case VarReference(variable: Variable) extends Token[C, P]
  case LParen() extends Token[C, P]
  case RParen() extends Token[C, P]
  case ArgBodySeparator() extends Token[C, P]
  case Lambda() extends Token[C, P]

abstract class ExtendedLambdaTermTokenizer[C, P] extends RegexParsers {
  def extensionTokenizer: Parser[Either[C, P]]

  def variableTokenizer: Parser[Variable] = """[a-zA-Z_][a-zA-Z0-9_]*""".r

  def tokenParser: Parser[Token[C, P]] =
    extensionTokenizer ^^ {
      case Left(constant)           => Token.Constant(constant)
      case Right(primitiveOperator) => Token.PrimitiveOperator(primitiveOperator)
    } |
      """[a-zA-Z_][a-zA-Z0-9_]*""".r ^^ { vName => Token.VarReference(vName) } |
      "(" ^^ { _ => Token.LParen() } |
      ")" ^^ { _ => Token.RParen() } |
      "." ^^ { _ => Token.ArgBodySeparator() } |
      "\\" ^^ { _ => Token.Lambda() } |
      "λ" ^^ { _ => Token.Lambda() }

  def tokenStreamParser: Parser[Vector[Token[C, P]]] = rep {
    """\s*""".r ~> tokenParser <~ """\s*""".r
  } ^^ Vector.from
}

object LambdaTermTokenizer extends ExtendedLambdaTermTokenizer[Nothing, Nothing] {
  def extensionTokenizer: Parser[Either[Nothing, Nothing]] = failure(
    "No extension token is defined by this implementation"
  )

  def tokenize(input: String): Either[String, Vector[Token[Nothing, Nothing]]] =
    parse(tokenStreamParser, input) match {
      case Success(tokens, _) => Right(tokens)
      case NoSuccess(msg, _)  => Left(msg)
      case Failure(msg, _)    => Left(msg)
      case Error(msg, _)      => Left(msg)
    }
}
