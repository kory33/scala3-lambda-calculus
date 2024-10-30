package io.github.kory33.scala3_lambda_calculus.cek.parser

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.*
import org.scalatest.prop.TableDrivenPropertyChecks
import io.github.kory33.scala3_lambda_calculus.cek.extensions.Natural

class ParserSpec extends AnyFreeSpec with Matchers with TableDrivenPropertyChecks with TestCases.TestCasesTables {
  "LambdaTermTokenizer + ExtendedLambdaTermParser" - {
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

  "ArithmeticTokenizer + ExtendedLambdaTermParser" - {
    val e2eTestCases = {
      import io.github.kory33.scala3_lambda_calculus.cek.ExtendedLambdaTerm.*
      import io.github.kory33.scala3_lambda_calculus.cek.extensions.ArithmeticOps.*

      Table(
        heading = "input" -> "expected",
        "[Add]" -> PrimitiveOperator(Add, Nil),
        "[Mul]" -> PrimitiveOperator(Mul, Nil),
        "[FirstLeqSecond]" -> PrimitiveOperator(FirstLeqSecond, Nil),
        "[If]" -> PrimitiveOperator(If, Nil),
        "123" -> Constant(Natural(123)),
        "true" -> Constant(true),
        "false" -> Constant(false),
        "[Add 1 2]" -> PrimitiveOperator(Add, List(Constant(Natural(1)), Constant(Natural(2)))),
        "[FirstLeqSecond 1 2]" -> PrimitiveOperator(FirstLeqSecond, List(Constant(Natural(1)), Constant(Natural(2)))),
        "[If false 1 2]" -> PrimitiveOperator(If, List(Constant(false), Constant(Natural(1)), Constant(Natural(2)))),
        "[If [FirstLeqSecond 1 2] 1 2]" ->
          PrimitiveOperator(
            If,
            List(
              PrimitiveOperator(FirstLeqSecond, List(Constant(Natural(1)), Constant(Natural(2)))),
              Constant(Natural(1)),
              Constant(Natural(2))
            )
          ),
        "(λx. [Mul [Mul x x] x]) 3" ->
          Application(
            Abstraction(
              "x",
              PrimitiveOperator(
                Mul,
                List(PrimitiveOperator(Mul, List(VarReference("x"), VarReference("x"))), VarReference("x"))
              )
            ),
            Constant(Natural(3))
          )
      )
    }

    "Succeeds in parsing end-to-end test cases" in forAll(e2eTestCases) { (input, expected) =>
      val tokenStream = ArithmeticTokenizer.tokenize(input).getOrElse(fail("Failed to tokenize"))
      val parsed = ExtendedLambdaTermParser.parse(tokenStream).getOrElse(fail("Failed to parse"))

      parsed mustBe expected
    }
  }
}
