package xyz.minond.talk.pti

trait Value

case class BooleanValue(value: Boolean) extends Value
case class ErrorValue(message: String) extends Value
case class IntNumberValue(value: Int) extends Value
case class LambdaValue(args: List[Expression], body: Expression) extends Value
case class ListValue(values: List[Value]) extends Value
case class RealNumberValue(value: Double) extends Value
case class StringValue(value: String) extends Value
case class SymbolValue(value: String) extends Value
case class VarValue(label: String, value: Value) extends Value

case class Environment(vars: Map[String, VarValue], parent: Option[Environment] = None) {
  def define(definition: VarValue) = {
    Environment(vars ++ Map(definition.label -> definition), parent)
  }

  def lookup(label: String): Value = {
    val err = VarValue("", ErrorValue(Interpreter.Message.ERR_UNDEFINED_LOOKUP(label)))
    vars.getOrElse(label, err).value
  }
}

object Interpreter {
  object Message {
    def ERR_UNDEFINED_LOOKUP(label: String) =
      s"${label} is undefined."
    def ERR_ARITY_MISMATCH(expected: Int, got: Int) =
      s"Arity mismatch. Expected ${expected} arguments but got ${got}."
    def ERR_INVALID_ADD(a: Value, b: Value) =
      s"Cannot add a(n) ${a} and a(n) ${b} together."
  }

  def eval(
      exprs: List[Either[Parser.Error, Expression]],
      origEnv: Environment): (List[Value], Environment) = {
    var env = origEnv
    (exprs.map { expr =>
      val (value, newEnv) = eval(expr, env)
      env = newEnv
      value
    }, env)
  }

  def eval(
      expr: Either[Parser.Error, Expression],
      env: Environment): (Value, Environment) = {
    expr match {
      case Right(BooleanExpr(value)) => (BooleanValue(value), env)
      case Right(IntNumberExpr(value)) => (IntNumberValue(value), env)
      case Right(RealNumberExpr(value)) => (RealNumberValue(value), env)
      case Right(StringExpr(value)) => (StringValue(value), env)

      case Right(QuoteExpr(SExpr(values))) =>
        (ListValue(safeEvals(values, env)), env)
      case Right(SExpr(IdentifierExpr("list") :: values)) =>
        (ListValue(safeEvals(values, env)), env)

      case Right(SExpr(IdentifierExpr("error") :: StringExpr(msg) :: Nil)) =>
        (ErrorValue(msg), env)

      case Right(SExpr(IdentifierExpr("lambda") :: SExpr(args) :: body :: Nil)) =>
        (LambdaValue(args, body), env)

      case Right(
          SExpr(IdentifierExpr("define") :: IdentifierExpr(name) :: value :: Nil)) =>
        val definition = VarValue(name, safeEval(value, env))
        (definition, env.define(definition))

      case Right(IdentifierExpr(label)) =>
        (env.lookup(label), env)

      // XXX Should be a generic function call
      case Right(SExpr(IdentifierExpr("equal?") :: values)) =>
        (builtinEquals(safeEvals(values, env)), env)

      // XXX Should be a generic function call
      case Right(SExpr(IdentifierExpr("+") :: values)) =>
        (builtinAdd(safeEvals(values, env)), env)

      // XXX Add toString method to all error classes
      case Left(_) => ???

      // XXX Finish all cases
      case _ => ???
    }
  }

  def safeEval(expr: Expression, env: Environment): Value =
    eval(Right(expr), env)._1

  def safeEvals(exprs: List[Expression], env: Environment): List[Value] =
    exprs.map { expr =>
      safeEval(expr, env)
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
            ErrorValue(Interpreter.Message.ERR_INVALID_ADD(a, b))
        }
    }
  }

  def builtinEquals(values: List[Value]): Value = {
    values match {
      case lhs :: rhs :: Nil =>
        BooleanValue(lhs == rhs)

      case _ => ErrorValue(Interpreter.Message.ERR_ARITY_MISMATCH(2, values.size))
    }
  }
}
