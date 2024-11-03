package io.github.kory33.scala3_lambda_calculus.extended

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import io.github.kory33.scala3_lambda_calculus.extended.parser.TestCases

class ShowExtendedLambdaTermSpec
    extends AnyFreeSpec
    with Matchers
    with TableDrivenPropertyChecks
    with TestCases.TestCasesTables {
  "LambdaTerm" - {
    import ExtendedLambdaTerm.*
    val testCases = Table(
      "ast" -> "expected",
      VarReference("a") -> "a",
      Application(VarReference("a"), VarReference("b")) -> "a b",
      Application(Application(VarReference("a"), VarReference("b")), VarReference("c")) -> "a b c",
      Application(VarReference("a"), Abstraction("b", VarReference("b"))) -> "a (\\b. b)"
    )

    "show testcases" in forAll(testCases) { (ast, expected) =>
      import cats.syntax.all.*

      ast.show shouldEqual expected
    }
  }

  "LambdaTerm -> String --[LambdaTermTokenizer -> ExtendedLambdaTermParser]-> LambdaTerm is identity" - {
    "on parsed terms" in {
      import io.github.kory33.scala3_lambda_calculus.extended.parser.{LambdaTermTokenizer, ExtendedLambdaTermParser}
      import cats.syntax.all.*

      forAll(positiveInputs) { input =>
        val tokenStream = LambdaTermTokenizer
          .tokenize(input)
          .getOrElse(fail("Failed to tokenize: input test case is not meant for this test"))
        val parsed = ExtendedLambdaTermParser
          .parse(tokenStream)
          .getOrElse(fail("Failed to parse: input test case is not meant for this test"))

        val tokenStreamOfShown = LambdaTermTokenizer.tokenize(parsed.show).getOrElse(fail("Failed to tokenize"))
        val parsedShown = ExtendedLambdaTermParser.parse(tokenStreamOfShown).getOrElse(fail("Failed to parse"))

        assert(parsedShown == parsed)
      }
    }
  }
}
