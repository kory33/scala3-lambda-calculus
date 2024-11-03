package io.github.kory33.scala3_lambda_calculus.extended.parser

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.*
import org.scalatest.prop.TableDrivenPropertyChecks

class TokenizerSpec extends AnyFreeSpec with Matchers with TableDrivenPropertyChecks with TestCases.TestCasesTables {
  "LambdaTermTokenizer" - {
    "Succeeds in tokenizing positive inputs" in forAll(positiveInputs) { input =>
      require(io.github.kory33.scala3_lambda_calculus.extended.parser.LambdaTermTokenizer.tokenize(input).isRight)
    }

    "Succeeds in tokenizing negative inputs (that must be rejected by parsers)" in forAll(negativeInputs.all) { input =>
      require(io.github.kory33.scala3_lambda_calculus.extended.parser.LambdaTermTokenizer.tokenize(input).isRight)
    }
  }
}
