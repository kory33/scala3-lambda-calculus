package io.github.kory33.scala3_lambda_calculus.extended

import cats.Show
import io.github.kory33.scala3_lambda_calculus.foundation.Variable
import cats.data.NonEmptyList
import scala.annotation.tailrec

object ShowExtendedLambdaTerm {
  import ExtendedLambdaTerm.*
  import cats.syntax.all.*

  enum SeqAmalgamatedTerm[C, P]:
    case VarReference(variable: Variable)
    case ApplicationSeq(term1: SeqAmalgamatedTerm[C, P], terms: NonEmptyList[SeqAmalgamatedTerm[C, P]])
    case AbstractionSeq(variables: NonEmptyList[Variable], body: SeqAmalgamatedTerm[C, P])
    case Constant(constant: C)
    case PrimitiveOperator(operator: P, arguments: List[SeqAmalgamatedTerm[C, P]])

  given [C: Show, P: Show]: Show[SeqAmalgamatedTerm[C, P]] with {
    def show(t: SeqAmalgamatedTerm[C, P]): String = {
      enum PrintPosition {
        case TopLevel, LambdaBody, Otherwise
      }

      def loop(t: SeqAmalgamatedTerm[C, P], position: PrintPosition): String = {
        t match {
          case SeqAmalgamatedTerm.VarReference(variable) => variable.toString
          case SeqAmalgamatedTerm.Constant(constant)     => constant.show
          case SeqAmalgamatedTerm.PrimitiveOperator(operator, arguments) =>
            s"[$operator ${arguments.map(loop(_, PrintPosition.Otherwise)).mkString(" ")}]"
          case SeqAmalgamatedTerm.ApplicationSeq(term1, terms) =>
            val content = (term1 :: terms.toList).map(loop(_, PrintPosition.Otherwise)).mkString(" ")
            position match
              case PrintPosition.TopLevel | PrintPosition.LambdaBody => content
              case PrintPosition.Otherwise                           => s"($content)"
          case SeqAmalgamatedTerm.AbstractionSeq(variables, body) =>
            val variablesStr = variables.toList.mkString(" ")
            val content = s"\\$variablesStr. ${loop(body, PrintPosition.LambdaBody)}"
            position match
              case PrintPosition.TopLevel => content
              case _                      => s"($content)"
        }
      }

      loop(t, PrintPosition.TopLevel)
    }

  }

  def amalgamate[C, P](term: ExtendedLambdaTerm[C, P]): SeqAmalgamatedTerm[C, P] = {
    term match {
      case VarReference(variable) => SeqAmalgamatedTerm.VarReference(variable)
      case Constant(constant)     => SeqAmalgamatedTerm.Constant(constant)
      case PrimitiveOperator(operator, arguments) =>
        SeqAmalgamatedTerm.PrimitiveOperator(operator, arguments.map(amalgamate))

      case Application(left, right) =>
        @tailrec def loop(
            toAmalgamateLeft: ExtendedLambdaTerm[C, P],
            accumArgs: NonEmptyList[SeqAmalgamatedTerm[C, P]]
        ): SeqAmalgamatedTerm[C, P] = {
          toAmalgamateLeft match {
            case Application(left, right) =>
              loop(left, amalgamate(right) :: accumArgs)
            case _ =>
              SeqAmalgamatedTerm.ApplicationSeq(amalgamate(toAmalgamateLeft), accumArgs)
          }
        }

        loop(left, NonEmptyList.of(amalgamate(right)))
      case Abstraction(boundVar, body) =>
        @tailrec def loop(
            toAmalgamateBody: ExtendedLambdaTerm[C, P],
            accumVars: NonEmptyList[Variable]
        ): SeqAmalgamatedTerm[C, P] = {
          toAmalgamateBody match {
            case Abstraction(boundVar, body) =>
              loop(body, boundVar :: accumVars)
            case _ =>
              SeqAmalgamatedTerm.AbstractionSeq(accumVars.reverse, amalgamate(toAmalgamateBody))
          }
        }

        loop(body, NonEmptyList.of(boundVar))
    }
  }

  def show[C: Show, P: Show](term: ExtendedLambdaTerm[C, P]): String = {
    val amalgamated = amalgamate(term)
    amalgamated.show
  }
}
