package xyz.minond.talk.pti

abstract class Value {
  override final def toString =
    this match {
      case BooleanValue(value) => if (value) "#t" else "#f"
      case err: ErrorValue => s"; Error:\n${err.stringify()}\n"
      case IntNumberValue(value) => value.toString
      case BuiltinValue(_) => "#<builtin>"
      case LambdaValue(_, _, _) => "#<procedure>"
      case LazyValue(expr) => s"'${expr}"
      case ListValue(values) => s"'(${values.map(_.toString).mkString(" ")})"
      case RealNumberValue(value) => value.toString
      case StringValue(value) => value
      case SymbolValue(value) => s"'$value"
      case VarValue(_, value) => value.toString
    }
}

case class BooleanValue(value: Boolean) extends Value
case class BuiltinValue(fn: (List[Expression], Environment) => Value) extends Value
case class IntNumberValue(value: Int) extends Value
case class LazyValue(expr: Expression) extends Value
case class ListValue(values: List[Value]) extends Value
case class RealNumberValue(value: Double) extends Value
case class StringValue(value: String) extends Value
case class SymbolValue(value: String) extends Value
case class VarValue(label: String, value: Value) extends Value

case class ErrorValue(message: String, prev: Option[ErrorValue] = None) extends Value {
  def stringify(prefix: String = ""): String = {
    val next = prev match {
      case Some(err) => "\n" + err.stringify(prefix + "  ")
      case None => ""
    }

    s"; ${prefix}- ${message}${next}"
  }
}

case class LambdaValue(args: Set[String], body: Expression, env: Environment)
    extends Value {
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
    def GIVEN(thing: String) =
      s"Given $thing."

    def ERR_SYNTAX(err: Parser.Error) =
      s"Syntax error:\n${err.stringify("  ")}"
    def ERR_BAD_SYNTAX(thing: String) =
      s"${thing}: lambda syntax"
    def ERR_EXPRESSION(expr: Expression) =
      s"Expression error: ${expr}"

    def ERR_UNDEFINED_LOOKUP(label: String) =
      s"${label} is undefined."
    def ERR_ARITY_MISMATCH(expected: Int, got: Int) =
      s"Arity mismatch. Expected ${expected} arguments but got ${got}."
    def ERR_INVALID_ERROR(values: List[Value]) =
      s"Cannot use `(${values.mkString(" ")})` as an error message."
    def ERR_INVALID_ADD(a: Value, b: Value) =
      s"Cannot add a(n) ${a} and a(n) ${b} together."

    def ERR_EVAL_EXPR(expr: Expression) =
      s"Error evaluating $expr."

    val ERR_LAMBDA_EMPTY_CALL =
      "Missing procedure expression"
    val ERR_LAMBDA_NON_PROC_CALL =
      "Call made to non-procedure."
    val ERR_LAMBDA_NON_ID_ARG =
      "Found non-identifier argument(s) in lambda expression."
    def ERR_LAMBDA_DUP_ARGS(dups: List[String]) =
      s"Found duplicate argument name(s): ${dups.mkString(", ")}."
  }

  val builtin = Map(
    "eval" -> BuiltinValue({ (args, env) =>
      args match {
        case IdentifierExpr(name) :: Nil =>
          env.lookup(name) match {
            case LazyValue(expr) => safeEval(expr, env)
            case res => res
          }

        case QuoteExpr(expr) :: Nil => safeEval(expr, env)
        case expr :: Nil => safeEval(expr, env)
        case Nil => ErrorValue(Message.ERR_ARITY_MISMATCH(1, 0))
        case exprs => ErrorValue(Message.ERR_ARITY_MISMATCH(1, exprs.size))
      }
    }),
    "cond" -> BuiltinValue({ (args, env) =>
      args.find {
        case SExpr(cond :: _) =>
          safeEval(cond, env) match {
            case BooleanValue(false) => false
            case _ => true
          }
      } match {
        case None => ListValue(List.empty)
        case Some(SExpr(_ :: Nil)) => ListValue(List.empty)
        case Some(SExpr(_ :: exprs)) => exprs.map(safeEval(_, env)).last
        case Some(expr) => ErrorValue(Message.ERR_EVAL_EXPR(expr))
      }
    }),
    "+" -> BuiltinValue({ (args, env) =>
      def aux(values: List[Value]): Value =
        values match {
          case Nil => IntNumberValue(0)
          case h :: rest =>
            (h, aux(rest)) match {
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

      aux(safeEval(args, env))
    }),
    "equal?" -> BuiltinValue({ (args, env) =>
      safeEval(args, env) match {
        case lhs :: rhs :: Nil => BooleanValue(lhs == rhs)
        case _ => ErrorValue(Message.ERR_ARITY_MISMATCH(2, args.size))
      }
    }),
    "list" -> BuiltinValue({ (args, env) =>
      ListValue(safeEval(args, env))
    }),
    "error" -> BuiltinValue({ (args, env) =>
      safeEval(args, env) match {
        case StringValue(msg) :: Nil => ErrorValue(msg)
        case SymbolValue(msg) :: Nil => ErrorValue(msg)
        case value => ErrorValue(Message.ERR_INVALID_ERROR(value))
      }
    }),
    "lambda" -> BuiltinValue({ (args, env) =>
      args match {
        case SExpr(raw) :: body :: Nil =>
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

          if (dups.size > 0) ErrorValue(Message.ERR_LAMBDA_DUP_ARGS(dups.toList))
          else if (errs.size > 0) ErrorValue(Message.ERR_LAMBDA_NON_ID_ARG)
          else LambdaValue(names.toSet, body, env)

        case _ =>
          ErrorValue(Message.ERR_BAD_SYNTAX("lambda"))
      }
    })
  )

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

      case Right(QuoteExpr(BooleanExpr(value))) => (BooleanValue(value), env)
      case Right(QuoteExpr(IdentifierExpr(name))) => (SymbolValue(name), env)
      case Right(QuoteExpr(IntNumberExpr(value))) => (IntNumberValue(value), env)
      case Right(QuoteExpr(RealNumberExpr(value))) => (RealNumberValue(value), env)
      case Right(QuoteExpr(StringExpr(value))) => (StringValue(value), env)
      case Right(QuoteExpr(value)) => (LazyValue(value), env)

      case Right(
          SExpr(IdentifierExpr("define") :: IdentifierExpr(name) :: value :: Nil)) =>
        define(name, value, env)
      case Right(SExpr(IdentifierExpr("define") :: _)) =>
        (ErrorValue(Message.ERR_BAD_SYNTAX("define")), env)

      case Right(IdentifierExpr(label)) =>
        (builtin.getOrElse(label, env.lookup(label)), env)

      case Right(SExpr(fn :: args)) => procCall(fn, args, env)
      case Right(SExpr(Nil)) => (ErrorValue(Message.ERR_LAMBDA_EMPTY_CALL), env)

      case Right(expr) => (ErrorValue(Message.ERR_EXPRESSION(expr)), env)
      case Left(err) => (ErrorValue(Message.ERR_SYNTAX(err)), env)
    }
  }

  def safeEval(expr: Expression, env: Environment): Value =
    eval(Right(expr), env)._1

  def safeEval(exprs: List[Expression], env: Environment): List[Value] =
    exprs.map { expr =>
      safeEval(expr, env)
    }

  def define(name: String, value: Expression, env: Environment): (Value, Environment) = {
    val definition = VarValue(name, safeEval(value, env))
    (definition, env.define(definition))
  }

  def procCall(
      fn: Expression,
      args: List[Expression],
      env: Environment): (Value, Environment) =
    safeEval(fn, env) match {
      case proc: LambdaValue =>
        if (!proc.validArity(args.size))
          (ErrorValue(Message.ERR_ARITY_MISMATCH(proc.args.size, args.size)), env)
        else
          // XXX Check that all arguments were evaluated
          (safeEval(proc.body, proc.scope(safeEval(args, env), proc.env, env)), env)

      case BuiltinValue(fn) => (fn(args, env), env)
      case err: ErrorValue => (err, env)

      case _ =>
        (
          ErrorValue(
            Message.ERR_LAMBDA_NON_PROC_CALL,
            Some(ErrorValue(Message.GIVEN(fn.toString)))),
          env)
    }
}
