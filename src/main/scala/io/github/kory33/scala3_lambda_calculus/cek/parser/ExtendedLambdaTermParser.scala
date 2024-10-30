package io.github.kory33.scala3_lambda_calculus.cek.parser

import scala.util.parsing.combinator.RegexParsers
import io.github.kory33.scala3_lambda_calculus.foundation.Variable
import cats.data.Chain
import io.github.kory33.scala3_lambda_calculus.cek.ExtendedLambdaTerm
import scala.util.parsing.combinator.Parsers
import io.github.kory33.scala3_lambda_calculus.util.parsing.VectorReader

/*
 * Syntax (tokens are defined in Tokenizer.scala):
 *  - <ExtendedLambdaTerm> ::= <Abstraction> | <PrimaryTerm>+
 *  - <PrimaryTerm>
 *     ::= <Variable>
 *       | <Constant>
 *       | <LParen> <ExtendedLambdaTerm> <RParen>
 *       | <LSquareParen> <PrimitiveOperator> <PrimaryTerm>* <RSquareParen>
 *  - <Abstraction> ::= <Lambda> <Variable>+ <ArgBodySeparator> <ExtendedLambdaTerm>
 *      (where <Variable> is a sequence of distinct variables)
 */
class ExtendedLambdaTermParser[C, P] extends Parsers {
  type Elem = Token[C, P]

  val variable = accept("variable", { case Token.VarReference(v) => v })
  val constant = accept("constant", { case Token.Constant(c) => ExtendedLambdaTerm.Constant[C, P](c) })
  val lparen = accept("left paren", { case Token.LParen() => () })
  val rparen = accept("right paren", { case Token.RParen() => () })
  val lsquareparen = accept("lsquareparen", { case Token.LSquareParen() => () })
  val rsquareparen = accept("rsquareparen", { case Token.RSquareParen() => () })
  val lambda = accept("lambda", { case Token.Lambda() => () })
  val argBodySeparator = accept("arg body separator", { case Token.ArgBodySeparator() => () })
  val primitiveOperator = accept("primitive operator", { case Token.PrimitiveOperator(p) => p })

  def term: Parser[ExtendedLambdaTerm[C, P]] =
    abstraction | rep1(primaryTerm) ^^ { terms =>
      terms.reduceLeft(ExtendedLambdaTerm.Application[C, P])
    }

  def primaryTerm: Parser[ExtendedLambdaTerm[C, P]] = {
    (variable ^^ ExtendedLambdaTerm.VarReference[C, P])
      | constant
      | (lparen ~> term <~ rparen)
      | {
        (lsquareparen ~> primitiveOperator) ~ (rep(primaryTerm) <~ rsquareparen) ^^ { case ~(op, args) =>
          ExtendedLambdaTerm.PrimitiveOperator(op, args)
        }
      }
  }

  def abstraction =
    ((lambda ~> rep1(variable)) ~ (argBodySeparator ~> term)).flatMap { case ~(variables, body) =>
      println(variables)

      if (variables.distinct.length != variables.length) {
        failure("Variables in abstraction must be distinct")
      } else {
        success(variables.foldRight(body) { case (variable, wrappedBody) =>
          ExtendedLambdaTerm.Abstraction[C, P](variable, wrappedBody)
        })
      }
    }
}

object ExtendedLambdaTermParser {
  def parse[C, P](input: Vector[Token[C, P]]): Either[String, ExtendedLambdaTerm[C, P]] = {
    val parser = new ExtendedLambdaTermParser[C, P]
    parser.term.apply(VectorReader(input)) match {
      case parser.Success(result, next) if next.atEnd => Right(result)
      case parser.Success(_, next)                    => Left(s"Unexpected token: ${next.first}")
      case parser.NoSuccess(msg, _)                   => Left(msg)
      case parser.Failure(msg, _)                     => Left(msg)
      case parser.Error(msg, _)                       => Left(msg)
    }
  }
}
