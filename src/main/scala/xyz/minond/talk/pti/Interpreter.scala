package xyz.minond.talk.pti

object Interpreter {
  object Message {
    def GIVEN(thing: String) =
      s"Given $thing."

    def ERR_SYNTAX(err: Parser.Error) =
      s"Syntax error:\n${err.stringify("  ")}"
    def ERR_BAD_SYNTAX(thing: String) =
      s"${thing}: bad syntax"
    def ERR_EXPRESSION(expr: Expression) =
      s"Expression error: ${expr}"

    def ERR_UNDEFINED_LOOKUP(label: String) =
      s"${label} is undefined."
    def ERR_ARITY_MISMATCH(expected: Int, got: Int) =
      s"Arity mismatch, expected ${expected} arguments but got ${got}."
    def ERR_INVALID_ERROR(exprs: List[Expression]) =
      s"Cannot use `(${exprs.mkString(" ")})` as an error message."
    def ERR_INVALID_ADD(a: Expression, b: Expression) =
      s"Cannot add a(n) ${a} and a(n) ${b} together."

    val ERR_INTERNAL =
      "Internal error"
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
    "eval" -> BuiltinExpr({ (args, env) =>
      safeEval(args, env) match {
        case expr :: Nil => safeEval(expr.unQuote, env)
        case Nil => ErrorExpr(Message.ERR_ARITY_MISMATCH(1, 0))
        case exprs => ErrorExpr(Message.ERR_ARITY_MISMATCH(1, exprs.size))
      }
    }),
    "cons" -> BuiltinExpr({ (args, env) =>
      safeEval(args, env) match {
        case head :: SExpr(tail) :: Nil => QuoteExpr(SExpr(head.unQuote :: tail))
        case head :: tail :: Nil => Pair(head.unQuote, tail.unQuote)
        case _ :: _ :: _ :: Nil => ErrorExpr(Message.ERR_ARITY_MISMATCH(2, args.size))
        case _ => ErrorExpr(Message.ERR_INTERNAL)
      }
    }),
    "cond" -> BuiltinExpr({ (args, env) =>
      args.find {
        case SExpr(cond :: _) =>
          safeEval(cond, env) match {
            case BooleanExpr(false) => false
            case _ => true
          }
      } match {
        case None => SExpr(List.empty)
        case Some(SExpr(_ :: Nil)) => SExpr(List.empty)
        case Some(SExpr(_ :: exprs)) => exprs.map(safeEval(_, env)).last
        case Some(expr) => ErrorExpr(Message.ERR_EVAL_EXPR(expr))
      }
    }),
    "+" -> BuiltinExpr({ (args, env) =>
      def aux(exprs: List[Expression]): Expression =
        exprs match {
          case Nil => IntNumberExpr(0)
          case h :: rest =>
            (h, aux(rest)) match {
              case (IntNumberExpr(a), IntNumberExpr(b)) =>
                IntNumberExpr(a + b)

              case (RealNumberExpr(a), RealNumberExpr(b)) =>
                RealNumberExpr(a + b)

              case (RealNumberExpr(a), IntNumberExpr(b)) =>
                RealNumberExpr(a + b.toDouble)

              case (IntNumberExpr(a), RealNumberExpr(b)) =>
                RealNumberExpr(a.toDouble + b)

              case (a, b) =>
                ErrorExpr(Message.ERR_INVALID_ADD(a, b))
            }
        }

      aux(safeEval(args, env))
    }),
    "equal?" -> BuiltinExpr({ (args, env) =>
      safeEval(args, env) match {
        case lhs :: rhs :: Nil => BooleanExpr(lhs == rhs)
        case _ => ErrorExpr(Message.ERR_ARITY_MISMATCH(2, args.size))
      }
    }),
    "list" -> BuiltinExpr({ (args, env) =>
      SExpr(safeEval(args, env))
    }),
    "error" -> BuiltinExpr({ (args, env) =>
      safeEval(args, env) match {
        case StringExpr(msg) :: Nil => ErrorExpr(msg)
        case QuoteExpr(IdentifierExpr(msg)) :: Nil => ErrorExpr(msg)
        case QuoteExpr(StringExpr(msg)) :: Nil => ErrorExpr(msg)
        case expr => ErrorExpr(Message.ERR_INVALID_ERROR(expr))
      }
    }),
    "lambda" -> BuiltinExpr({ (args, env) =>
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

          if (dups.size > 0) ErrorExpr(Message.ERR_LAMBDA_DUP_ARGS(dups.toList))
          else if (errs.size > 0) ErrorExpr(Message.ERR_LAMBDA_NON_ID_ARG)
          else LambdaExpr(names.toSet, body, env)

        case _ =>
          ErrorExpr(Message.ERR_BAD_SYNTAX("lambda"))
      }
    })
  )

  def eval(
      exprs: List[Either[Parser.Error, Expression]],
      origEnv: Environment): (List[Expression], Environment) = {
    var env = origEnv
    (exprs.map { expr =>
      val (value, newEnv) = eval(expr, env)
      env = newEnv
      value
    }, env)
  }

  def eval(
      expr: Either[Parser.Error, Expression],
      env: Environment): (Expression, Environment) = {
    expr match {
      case Right(BooleanExpr(value)) => (BooleanExpr(value), env)
      case Right(IntNumberExpr(value)) => (IntNumberExpr(value), env)
      case Right(RealNumberExpr(value)) => (RealNumberExpr(value), env)
      case Right(StringExpr(value)) => (StringExpr(value), env)

      case Right(QuoteExpr(IdentifierExpr(name))) =>
        (QuoteExpr(IdentifierExpr(name)), env)
      case Right(QuoteExpr(value)) => (value, env)

      case Right(
          SExpr(IdentifierExpr("define") :: IdentifierExpr(name) :: value :: Nil)) =>
        define(name, value, env)
      case Right(SExpr(IdentifierExpr("define") :: _)) =>
        (ErrorExpr(Message.ERR_BAD_SYNTAX("define")), env)

      case Right(IdentifierExpr(label)) =>
        (builtin.getOrElse(label, env.lookup(label)), env)

      case Right(SExpr(fn :: args)) => procCall(fn, args, env)
      case Right(SExpr(Nil)) => (ErrorExpr(Message.ERR_LAMBDA_EMPTY_CALL), env)

      case Right(expr) => (ErrorExpr(Message.ERR_EXPRESSION(expr)), env)
      case Left(err) => (ErrorExpr(Message.ERR_SYNTAX(err)), env)
    }
  }

  def safeEval(expr: Expression, env: Environment): Expression =
    eval(Right(expr), env)._1

  def safeEval(exprs: List[Expression], env: Environment): List[Expression] =
    exprs.map { expr =>
      safeEval(expr, env)
    }

  def define(
      name: String,
      value: Expression,
      env: Environment): (Expression, Environment) = {
    val res = safeEval(value, env)
    (res, env.define(name, res))
  }

  def procCall(
      fn: Expression,
      args: List[Expression],
      env: Environment): (Expression, Environment) =
    safeEval(fn, env) match {
      case proc: LambdaExpr =>
        if (!proc.validArity(args.size))
          (ErrorExpr(Message.ERR_ARITY_MISMATCH(proc.args.size, args.size)), env)
        else
          // XXX Check that all arguments were evaluated
          (safeEval(proc.body, proc.scope(safeEval(args, env), proc.env, env)), env)

      case BuiltinExpr(fn) => (fn(args, env), env)
      case err: ErrorExpr => (err, env)

      case _ =>
        (
          ErrorExpr(
            Message.ERR_LAMBDA_NON_PROC_CALL,
            Some(ErrorExpr(Message.GIVEN(fn.toString)))),
          env)
    }
}
