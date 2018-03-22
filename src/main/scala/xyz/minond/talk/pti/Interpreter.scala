package xyz.minond.talk.pti

trait Value

case class BooleanValue(value: Boolean) extends Value
case class ErrorValue(message: String) extends Value
case class IntNumberValue(value: Int) extends Value
case class LambdaValue(label: String) extends Value
case class ListValue(values: List[Value]) extends Value
case class RealNumberValue(value: Double) extends Value
case class StringValue(value: String) extends Value
case class SymbolValue(value: String) extends Value
case class VarValue(label: String) extends Value

object Interpreter {
  def eval(expr: Either[Parser.Error, Expression]): Value = {
    expr match {
      case Right(BooleanExpr(value)) => BooleanValue(value)
      case Right(IntNumberExpr(value)) => IntNumberValue(value)
      case Right(RealNumberExpr(value)) => RealNumberValue(value)
      case Right(StringExpr(value)) => StringValue(value)

      case Right(QuoteExpr(SExpr(values))) => ListValue(safeEvals(values))
      case Right(SExpr(IdentifierExpr("list") :: values)) => ListValue(safeEvals(values))

      case Right(SExpr(IdentifierExpr("error") :: StringExpr(msg) :: Nil)) =>
        ErrorValue(msg)

      case Right(SExpr(IdentifierExpr("equal?") :: values)) =>
        builtinEquals(safeEvals(values))

      case Right(SExpr(IdentifierExpr("+") :: values)) =>
        builtinAdd(safeEvals(values))

      // XXX Add toString method to all error classes
      case Left(_) => ???
      case _ => ???
    }
  }

  def safeEval(expr: Expression): Value =
    eval(Right(expr))

  def evals(exprs: List[Either[Parser.Error, Expression]]): List[Value] =
    exprs.map { expr =>
      eval(expr)
    }

  def safeEvals(exprs: List[Expression]): List[Value] =
    exprs.map { expr =>
      safeEval(expr)
    }

  def builtinAdd(values: List[Value]): Value = {
    values match {
      case Nil => IntNumberValue(0)
      case h :: rest =>
        (h, builtinAdd(rest)) match {
          case (IntNumberValue(a), IntNumberValue(b)) =>
            IntNumberValue(a + b)

          case (RealNumberValue(a), RealNumberValue(b)) =>
            RealNumberValue(a + b)

          case (RealNumberValue(a), IntNumberValue(b)) =>
            RealNumberValue(a + b.toDouble)

          case (IntNumberValue(a), RealNumberValue(b)) =>
            RealNumberValue(a.toDouble + b)

          case (a, b) =>
            ErrorValue(s"Cannot add a(n) ${a} and a(n) ${b} together.")
        }
    }
  }

  def builtinEquals(values: List[Value]): Value = {
    values match {
      case lhs :: rhs :: Nil =>
        BooleanValue(lhs == rhs)

      case _ => ErrorValue(s"Arity mismatch. Expected 2 but got ${values.size}")
    }
  }
}
