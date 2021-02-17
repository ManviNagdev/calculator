package manvi
import scala.io.StdIn.readLine
import atto.Atto._
import atto._
import ParseResult._

object Calculator extends App {

  // Grammar for solving airthmetic expressions

  // Expr is a sequence of terminals and nonterminals that starts with a term
  // Expr -> Term Expr'
  lazy val Expr: Parser[Double] = for {
    lhs <- Term
    next <- Expr_prim // sequence of nonterminals and terminals
  } yield next(lhs)

  // Expr' is a sequence of terminals and nonterminals that starts with + or - or is empty
  // Expr' -> + Term Expr' | - Term Expr' | ε
  lazy val Expr_prim: Parser[Double => Double] = {

    // for adding a terminal or the value of expression to the left term
    val p1 = for {
      _ <- char('+')
      rhs <- Term
      next <- Expr_prim
    } yield (lhs: Double) => next(lhs + rhs)

    // for subtracting a terminal or the value of expression from the left term
    val p2 = for {
      _ <- char('-')
      rhs <- Term
      next <- Expr_prim
    } yield (lhs: Double) => next(lhs - rhs)

    // if no terms on the right
    val p3 = ok((lhs: Double) => lhs)
    p1 | p2 | p3
  }

  // Term -> Factor Term'
  lazy val Term: Parser[Double] = for {
    lhs <- Factor
    next <- Term_prim
  } yield next(lhs)

  // Term' is a sequence of terminals and nonterminals that starts with * or / or is empty
  // Term' -> * Factor Term' | / Factor Term' | ε
  lazy val Term_prim: Parser[Double => Double] = {

    // for multiplying a terminal or the value of expression to the left term
    val p1 = for {
      _ <- char('*')
      rhs <- Factor
      next <- Term_prim
    } yield (lhs: Double) => next(lhs * rhs)

    // for dividing the left term by a terminal or the value of expression
    val p2 = for {
      _ <- char('/')
      rhs <- Factor
      next <- Term_prim
    } yield (lhs: Double) => next(lhs / rhs)

    // if no terms on the right
    val p3 = ok((lhs: Double) => lhs)
    p1 | p2 | p3
  }

  // Factor is a number or an expression in parenthesis
  // Factor -> number | (Expr)
  lazy val Factor: Parser[Double] = {
    val p1 = for {
      _ <- char('(')
      e <- Expr
      _ <- char(')')
    } yield e
    val p2 = double
    p1 | p2
  }

  // driver function. Returns string because I needed it to return "invalid expression format" whenever applicable instead of the answer.
  def calculate(e: String): String = {
    def doneToDouble(pr: ParseResult[Double]): String =
      pr match {
        case Done(remaining, answer) =>
          remaining match {
            case "" => "" + answer
            // case _  => "invalid expression format " + remaining // for debugging
            case _ => "invalid expression format"
          }
        // case Fail(remaining, stack, failure) =>
        //   "invalid expression format " + remaining + " " + failure + " " + stack // for debugging
        case Fail(remaining, _, failure) => "invalid expression format"
        case Partial(_)                  => "invalid expression format"
      }
    return doneToDouble(
      Expr.parse(e.replaceAll("\\s", "")).done
    ) // eliminates stray spaces and calls the parser. Then, it sends the result to a function which extracts the final answer and returns it as a string
  }

}
