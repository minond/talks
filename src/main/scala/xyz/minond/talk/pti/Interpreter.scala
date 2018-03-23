package xyz.minond.talk.pti

trait Value

case class BooleanValue(value: Boolean) extends Value
case class ErrorValue(message: String) extends Value
case class IntNumberValue(value: Int) extends Value
case class ListValue(values: List[Value]) extends Value
case class RealNumberValue(value: Double) extends Value
case class StringValue(value: String) extends Value
case class SymbolValue(value: String) extends Value
case class VarValue(label: String, value: Value) extends Value

case class LambdaValue(args: Set[String], body: Expression, env: Environment) extends Value {
  def scope(vals: List[Value], local: Environment, global: Environment): Environment =
    // XXX Add support for varargs
    args.zip(vals).foldLeft[Environment](local.pushBack(global)) {
      case (env: Environment, (name: String, value: Value)) =>
        env.define(VarValue(name, value))

      case _ => local
    }

  def validArity(count: Int): Boolean =
    // XXX Add support for varargs
    count == args.size
}

case class Environment(vars: Map[String, VarValue], parent: Option[Environment] = None) {
  def define(definition: VarValue) = {
    Environment(vars ++ Map(definition.label -> definition), parent)
  }

  def pushBack(env: Environment): Environment = {
    parent match {
      case Some(par) => Environment(vars, Some(par.pushBack(env)))
      case None => Environment(vars, Some(env))
    }
  }

  def lookup(label: String): Value = {
    (vars.get(label), parent) match {
      case (Some(varval), _) => varval.value
      case (None, Some(env)) => env.lookup(label)
      case (None, None) => ErrorValue(Interpreter.Message.ERR_UNDEFINED_LOOKUP(label))
    }
  }

  override def toString = {
    val text = vars.keys.toList ++ (parent match {
      case Some(env) => List(env.toString)
      case _ => List.empty
    })

    s"Environment{${text.mkString(", ")}}"
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

    val ERR_LAMBDA_NON_PROC_CALL =
      "Call made to non-procedure."
    val ERR_LAMBDA_NON_ID_ARG =
      "Found non-identifier argument(s)"
    def ERR_LAMBDA_DUP_ARGS(dups: List[String]) =
      s"Found duplicate argument name(s): ${dups.mkString(", ")}"
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

      case Right(SExpr(IdentifierExpr("lambda") :: SExpr(raw) :: body :: Nil)) =>
        val (args, errs) = raw.partition {
          case IdentifierExpr(_) => true
          case _ => false
        }

        val names = args.map {
          case IdentifierExpr(name) => name
        }

        val dups = names.groupBy(identity) collect {
          case (x, xs) if xs.size > 1 => x
        }

        if (dups.size > 0) (ErrorValue(Message.ERR_LAMBDA_DUP_ARGS(dups.toList)), env)
        else if (errs.size > 0) (ErrorValue(Message.ERR_LAMBDA_NON_ID_ARG), env)
        else (LambdaValue(names.toSet, body, env), env)

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

      case Right(SExpr(fn :: args)) =>
        safeEval(fn, env) match {
          case proc: LambdaValue =>
            if (!proc.validArity(args.size))
              (ErrorValue(Message.ERR_ARITY_MISMATCH(proc.args.size, args.size)), env)
            else {
              // XXX Check that all arguments were evaluated
              val vals = safeEvals(args, env)
              (safeEval(proc.body, proc.scope(vals, proc.env, env)), env)
            }

          case err: ErrorValue => (err, env)
          case invalid @ _ => (ErrorValue(Message.ERR_LAMBDA_NON_PROC_CALL), env)
        }

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
            ErrorValue(Message.ERR_INVALID_ADD(a, b))
        }
    }
  }

  def builtinEquals(values: List[Value]): Value = {
    values match {
      case lhs :: rhs :: Nil =>
        BooleanValue(lhs == rhs)

      case _ => ErrorValue(Message.ERR_ARITY_MISMATCH(2, values.size))
    }
  }
}
