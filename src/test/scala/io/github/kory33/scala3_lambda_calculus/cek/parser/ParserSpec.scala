package io.github.kory33.scala3_lambda_calculus.cek.parser

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.*
import org.scalatest.prop.TableDrivenPropertyChecks

class ParserSpec extends AnyFreeSpec with Matchers with TableDrivenPropertyChecks with TestCases.TestCasesTables {
  "Tokenizer + ExtendedLambdaTermParser" - {
    "Succeeds in parsing positive inputs" in forAll(positiveInputs) { input =>
      val tokenStream = LambdaTermTokenizer.tokenize(input).getOrElse(fail("Failed to tokenize"))
      require(ExtendedLambdaTermParser.parse(tokenStream).isRight)
    }

    "Fails in parsing negative inputs" in forAll(negativeInputs.syntaxError) { input =>
      val tokenStream = LambdaTermTokenizer.tokenize(input).getOrElse(fail("Failed to tokenize"))

      require(ExtendedLambdaTermParser.parse(tokenStream).isLeft)
    }

    val e2eTestCases = {
      import io.github.kory33.scala3_lambda_calculus.cek.ExtendedLambdaTerm.*
      Table(
        heading = "input" -> "expected",
        "a" -> VarReference("a"),
        "a b" -> Application(VarReference("a"), VarReference("b")),
        "a b c" -> Application(Application(VarReference("a"), VarReference("b")), VarReference("c")),
        "a (b c) d" -> Application(
          Application(VarReference("a"), Application(VarReference("b"), VarReference("c"))),
          VarReference("d")
        ),
        "\\a b c. b" -> Abstraction("a", Abstraction("b", Abstraction("c", VarReference("b")))),
        "(\\x. x)(\\y x. x)" -> Application(
          Abstraction("x", VarReference("x")),
          Abstraction("y", Abstraction("x", VarReference("x")))
        )
      )
    }

    "Succeeds in parsing end-to-end test cases" in forAll(e2eTestCases) { (input, expected) =>
      val tokenStream = LambdaTermTokenizer.tokenize(input).getOrElse(fail("Failed to tokenize"))
      val parsed = ExtendedLambdaTermParser.parse(tokenStream).getOrElse(fail("Failed to parse"))

      parsed mustBe expected
    }
  }
}
