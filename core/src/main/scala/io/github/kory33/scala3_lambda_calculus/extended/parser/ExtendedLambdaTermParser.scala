package io.github.kory33.scala3_lambda_calculus.extended.parser

import scala.util.parsing.combinator.RegexParsers
import io.github.kory33.scala3_lambda_calculus.foundation.Variable
import cats.data.Chain
import io.github.kory33.scala3_lambda_calculus.extended.ExtendedLambdaTerm
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

  def expect[U](expected: String, f: PartialFunction[Elem, U]): Parser[U] = Parser { in =>
    if (in.atEnd) Failure(s"Expected $expected, encountered end of input", in)
    else if (f.isDefinedAt(in.first)) Success(f(in.first), in.rest)
    else Failure(expected + " expected", in)
  }

  def expectAny(expected: String): Parser[Elem] = Parser { in =>
    if (in.atEnd) Failure(s"Expected $expected, encountered end of input", in)
    else Success(in.first, in.rest)
  }

  val variable = expect("a variable", { case Token.VarReference(v) => v })
  val constant = expect("a constant", { case Token.Constant(c) => ExtendedLambdaTerm.Constant[C, P](c) })
  val lparen = expect("a left paren (`(`)", { case Token.LParen() => () })
  val rparen = expect("a right paren (`)`)", { case Token.RParen() => () })
  val lsquareparen = expect("a left sq paren (`[`)", { case Token.LSquareParen() => () })
  val rsquareparen = expect("a right sq paren (`]`)", { case Token.RSquareParen() => () })
  val lambda = expect("a lambda symbol (`λ`, `\\`)", { case Token.Lambda() => () })
  val argBodySeparator = expect("the arg body separator (`.`)", { case Token.ArgBodySeparator() => () })
  val primitiveOperator = expect("a primitive operator", { case Token.PrimitiveOperator(p) => p })

  def term: Parser[ExtendedLambdaTerm[C, P]] =
    abstraction | rep1(primaryTerm) ^^ { terms =>
      terms.reduceLeft(ExtendedLambdaTerm.Application[C, P])
    }

  def primaryTerm: Parser[ExtendedLambdaTerm[C, P]] = {
    (variable ^^ ExtendedLambdaTerm.VarReference[C, P])
      | constant
      | (lparen ~> commit(term <~ rparen))
      | {
        lsquareparen ~> commit {
          primitiveOperator ~ (rep(primaryTerm) <~ rsquareparen) ^^ { case ~(op, args) =>
            ExtendedLambdaTerm.PrimitiveOperator(op, args)
          }
        }
      }
      | (expectAny("a primary term (variable / constant / parenthesized expression)").flatMap { token =>
        failure(s"Unexpected token: $token")
      })
  }

  def abstraction =
    lambda ~> commit {
      (rep1(variable) ~ (argBodySeparator ~> term)).flatMap { case ~(variables, body) =>
        if (variables.distinct.length != variables.length) {
          val duplicates = variables.groupBy(identity).filter(_._2.length > 1).keys
          failure(
            "Variables in abstraction must be distinct, but variable(s) { " +
              duplicates.mkString(", ") +
              " } are duplicated"
          )
        } else {
          success(variables.foldRight(body) { case (variable, wrappedBody) =>
            ExtendedLambdaTerm.Abstraction[C, P](variable, wrappedBody)
          })
        }
      }
    }
}

object ExtendedLambdaTermParser {
  def parse[C, P](input: Vector[Token[C, P]]): Either[String, ExtendedLambdaTerm[C, P]] = {
    val parser = new ExtendedLambdaTermParser[C, P]
    parser.term.apply(VectorReader(input)) match {
      case parser.Success(result, next) if next.atEnd => Right(result)
      case parser.Success(_, next)                    => Left(s"Unexpected token: ${next.first}")
      case parser.Failure(msg, _)                     => Left(msg)
      case parser.Error(msg, _)                       => Left(msg)
    }
  }
}
