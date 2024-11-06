package io.github.kory33.scala3_lambda_calculus.cek

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.*
import org.scalatest.prop.TableDrivenPropertyChecks
import io.github.kory33.scala3_lambda_calculus.extended.extensions.Natural
import io.github.kory33.scala3_lambda_calculus.extended.ExtendedLambdaTerm.*
import io.github.kory33.scala3_lambda_calculus.extended.parser.ArithmeticTokenizer
import io.github.kory33.scala3_lambda_calculus.extended.parser.ExtendedLambdaTermParser
import io.github.kory33.scala3_lambda_calculus.extended.ExtendedLambdaTerm
import io.github.kory33.scala3_lambda_calculus.extended.extensions.BoolOrNat
import io.github.kory33.scala3_lambda_calculus.extended.extensions.ArithmeticOps

class CEKMachineE2ETest extends AnyFreeSpec with Matchers with TableDrivenPropertyChecks {
  "Arithmetic CEK Machine (E2E test)" - {
    def parseOrFail(input: String): ExtendedLambdaTerm[BoolOrNat, ArithmeticOps] = {
      val tokens = ArithmeticTokenizer.tokenize(input) match {
        case Left(err)     => fail(err)
        case Right(tokens) => tokens
      }
      ExtendedLambdaTermParser.parse(tokens) match {
        case Left(err)   => fail(err)
        case Right(term) => term
      }
    }

    def evaluateE2EOrFail(input: String, stepLimit: Int = 1000): ExtendedLambdaTerm[BoolOrNat, ArithmeticOps] = {
      val finalState = CEKMachine(parseOrFail(input)).runUntilTermination(stepLimit = Some(stepLimit)) match {
        case Left(err)    => fail(err.toString)
        case Right(state) => state
      }

      finalState.closureToEvaluate.lambdaTerm
    }

    "evaluates primitive operators as in Scala" in forAll(
      Table(
        "input term" -> "expected term",
        "[Add 1 2]" -> s"${1 + 2}",
        "[Mul 2 3]" -> s"${2 * 3}",
        "[FirstLeqSecond 1 2]" -> s"${1 <= 2}",
        "[FirstLeqSecond 2 1]" -> s"${2 <= 1}",
        "[If true 1 2]" -> s"${if (true) 1 else 2}",
        "[If false 1 2]" -> s"${if (false) 1 else 2}"
      )
    ) { (input, expected) => evaluateE2EOrFail(input) mustBe parseOrFail(expected) }

    "evaluates nested primitive operators as in Scala" in forAll(
      Table(
        "input term" -> "expected term",
        "[If [FirstLeqSecond 1 2] 3 4]" -> s"${if (1 <= 2) 3 else 4}",
        "[Add [Mul 2 3] 4]" -> s"${(2 * 3) + 4}",
        "[Mul [Add [If false 1 2] 3] 4]" -> s"${((if (false) 1 else 2) + 3) * 4}"
      )
    ) { (input, expected) => evaluateE2EOrFail(input) mustBe parseOrFail(expected) }

    "preserves variable binding across primive operator application" in {
      evaluateE2EOrFail("(λn. [If true (λx. n) 0] (λy. y)) 5") mustBe parseOrFail(s"${5}")
    }

    "preserves environment formed within primitive operator argument" in {
      evaluateE2EOrFail("[If true ((λn x. n) 3) false] (λx. x)") mustBe parseOrFail(s"${3}")
    }

    "correctly evaluates 4!" in {
      val Z = "λf. (λx. f (λy. x x y)) (λx. f (λy. x x y))"
      val id = "λx. x"
      val factKernel = s"λf. λn. ([If [FirstLeqSecond n 1] (λthen. 1) (λelse. [Mul n (f [PredOrZero n])])]) ($id)"
      val fact4 = s"($Z) ($factKernel) 4"

      evaluateE2EOrFail(fact4) mustBe parseOrFail(s"${24}")
    }
  }
}
