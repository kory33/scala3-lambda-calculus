package io.github.kory33.scala3_lambda_calculus.extended.parser

import org.scalatest.prop.Tables

object TestCases {
  val positiveInputs = Seq(
    "a",
    "a b",
    "a b c",
    "a (b c) d",
    "a (   (b) c) d",
    "  (a b) c",
    "a (b c)",
    "(a ((a) b) c)",
    "\\a.\\b.a",
    "\\   a  .  \\b . a   ",
    "λa. λb. a",
    "λa.\\ b. a",
    "λx y. x",
    "λ   x   . λ   y   . x",
    "λa.λb.a",
    "λa.λb.λc.c",
    "λa.(λb.a)",
    "(λa.(λb.a))",
    "(λa.λb.a)(λc.c)",
    "(λa.λb.a) (λc.c)",
    "(λa.λb.a) (λc.c) (λd.d)",
    "λe.(λa.λb.a) (λc.c) (λd.d)"
  )

  // should pass the tokenizer but fail the parser
  object negativeInputs {
    val syntaxError = Seq(
      "",
      "a.",
      "λ.",
      "λ.a",
      "aλb.b",
      "a \\b. b",
      "aλb.b a",
      "(aλb.b)a",
      "λa(b).a",
      "λ(a)(b).a",
      "λ(a b).a",
      "a.\\b.a",
      "..",
      "\\.a",
      "\\aλ.a",
      "λλ",
      "\\\\",
      "\\a.\\b.a\\c.c",
      "λa",
      "aλa",
      "λ(a)",
      "λ(a).",
      "(λa.)a",
      "(λ)a.a",
      "(",
      "a b c)",
      "(a b c",
      "a )(b c"
    )

    val duplicateBindsInASingleLambda = Seq(
      "λa a.a",
      "\\a a.a"
    )

    val all = syntaxError ++ duplicateBindsInASingleLambda
  }

  trait TestCasesTables { self: Tables =>
    val positiveInputs = Table("input", TestCases.positiveInputs*)
    object negativeInputs {
      val syntaxError = Table("input", TestCases.negativeInputs.syntaxError*)
      val duplicateBindsInASingleLambda = Table("input", TestCases.negativeInputs.duplicateBindsInASingleLambda*)

      val all = Table("input", TestCases.negativeInputs.all*)
    }
  }
}
