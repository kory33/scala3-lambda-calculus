package io.github.kory33.scala3_lambda_calculus

import io.github.kory33.scala3_lambda_calculus.extended.parser.ArithmeticTokenizer
import io.github.kory33.scala3_lambda_calculus.extended.parser.ExtendedLambdaTermParser
import cats.Show
import io.github.kory33.scala3_lambda_calculus.extended.ExtendedLambdaTerm
import io.github.kory33.scala3_lambda_calculus.extended.extensions.BoolOrNat
import io.github.kory33.scala3_lambda_calculus.extended.extensions.ArithmeticOps
import io.github.kory33.scala3_lambda_calculus.cek.CEKMachineState
import io.github.kory33.scala3_lambda_calculus.cek.Environment

object CLI {
  import cats.syntax.all.*
  import io.github.kory33.scala3_lambda_calculus.extended.extensions.BoolOrNat.{*, given}

  object Printing {
    def printIndented[T: Show](indentCount: Int, t: T): Unit = {
      val s = t.show
      val indent = "  " * indentCount
      print(s.linesIterator.map(indent + _).mkString("\n"))
    }

    def printlnIndented[T: Show](indentCount: Int, t: T): Unit = {
      printIndented(indentCount, t)
      println()
    }

    def encloseInSGR(code: Int, text: String): String = s"\u001B[${code}m$text\u001B[0m"
  }
  import Printing.*

  def handleCEKMachineEvaluation(term: ExtendedLambdaTerm[BoolOrNat, ArithmeticOps]): Unit = {
    import io.github.kory33.scala3_lambda_calculus.cek.{*, given}
    import CEKMachineState.{*, given}
    import Continuation.*

    var machineState = CEKMachineState(Closure(term, Environment.empty), ThenHalt())

    val maxSteps = 2000
    var counter = 0
    while (counter < maxSteps) {
      if (machineState.isHaltingState) {
        printlnIndented(1, encloseInSGR(32, "[Evaluation terminated successfully]"))
        printlnIndented(2, encloseInSGR(32, s"${machineState.closureToEvaluate.lambdaTerm.show}"))
        return
      } else {
        printlnIndented(
          1,
          encloseInSGR(2, s"[Eval, step $counter]> ") + encloseInSGR(36, s"${machineState.show}")
        )

        machineState = machineState.stepOnce match {
          case Left(EvaluationError.VariableNotBound(v)) =>
            printlnIndented(2, encloseInSGR(31, s"Error: tried to evaluate a variable `$v` which is not bound"))
            return
          case Left(EvaluationError.ArgumentAppliedToConstant(c, arg)) =>
            printlnIndented(2, encloseInSGR(31, s"Error: tried to apply an argument `$arg` to a constant `$c`"))
            return
          case Left(EvaluationError.PrimitiveOperatorFailedToEvaluate(msg, op, args)) =>
            printlnIndented(
              2,
              encloseInSGR(
                31,
                s"Error: failed to evaluate a primitive operator `${op.show}` with arguments ${args.show}: $msg"
              )
            )
            return
          case Right(nextState) => nextState
        }
        counter += 1
      }
    }

    printlnIndented(1, encloseInSGR(31, s"Evaluation did not terminate within $maxSteps steps. Final state:"))
    printlnIndented(2, encloseInSGR(31, s"${machineState.show}"))
  }

  def handleExpressionEvaluation(input: String): Unit = {
    val tokenized = ArithmeticTokenizer.tokenize(input) match {
      case Right(tokens) => tokens
      case Left(msg) =>
        printlnIndented(1, encloseInSGR(31, s"Tokenization error: $msg"))
        return
    }

    val parsed = ExtendedLambdaTermParser.parse(tokenized) match {
      case Right(parsed) => parsed
      case Left(msg) =>
        printlnIndented(1, encloseInSGR(31, s"Parse error: $msg"))
        return
    }

    handleCEKMachineEvaluation(parsed)
  }

  def main(args: Array[String]): Unit = {
    val scanner = java.util.Scanner(System.in)

    while (true) {
      printIndented(0, encloseInSGR(32, encloseInSGR(2, "(input to evaluate)") + "> "))
      val input = scanner.nextLine()
      if (input == ":q") {
        return
      }

      handleExpressionEvaluation(input)
    }
  }
}
