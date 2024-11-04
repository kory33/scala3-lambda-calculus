package io.github.kory33.scala3_lambda_calculus.extended.parser

import io.github.kory33.scala3_lambda_calculus.foundation.Variable
import scala.util.parsing.combinator.RegexParsers

/** Tokens:
  *   - <LParen> ::= '('
  *   - <RParen> ::= ')'
  *   - <LSquareParen> ::= '['
  *   - <RSquareParen> ::= ']'
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
  case LSquareParen() extends Token[C, P]
  case RSquareParen() extends Token[C, P]
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
      "[" ^^ { _ => Token.LSquareParen() } |
      "]" ^^ { _ => Token.RSquareParen() } |
      "(" ^^ { _ => Token.LParen() } |
      ")" ^^ { _ => Token.RParen() } |
      "." ^^ { _ => Token.ArgBodySeparator() } |
      "\\" ^^ { _ => Token.Lambda() } |
      "λ" ^^ { _ => Token.Lambda() }

  def tokenStreamParser: Parser[Vector[Token[C, P]]] = rep {
    """\s*""".r ~> tokenParser <~ """\s*""".r
  } ^^ Vector.from

  def tokenize(input: String): Either[String, Vector[Token[C, P]]] =
    parse(tokenStreamParser, input) match {
      case Success(tokens, restInput) if restInput.atEnd => Right(tokens)
      case Success(_, restInput) => Left(s"Failed to tokenize the input:\n${restInput.pos.longString}")
      case Failure(msg, _)       => Left(msg)
      case Error(msg, _)         => Left(msg)
    }
}

object LambdaTermTokenizer extends ExtendedLambdaTermTokenizer[Nothing, Nothing] {
  def extensionTokenizer: Parser[Either[Nothing, Nothing]] = failure(
    "No extension token is defined by this implementation"
  )
}
