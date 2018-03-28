package xyz.minond.talk.pti

import java.nio.charset.Charset
import java.nio.file.{Files, Paths}
import scala.util.{Try, Failure, Success}

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
    def ERR_BAD_ARGS(args: String*) =
      s"Bad arguments, expecting ${args.mkString(", ")}"
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
    "eval" -> Builtin({ (args, env) =>
      safeEval(args, env) match {
        case expr :: Nil => safeEval(expr.unQuote, env)
        case Nil => Error(Message.ERR_ARITY_MISMATCH(1, 0))
        case exprs => Error(Message.ERR_ARITY_MISMATCH(1, exprs.size))
      }
    }),
    "cons" -> Builtin({ (args, env) =>
      safeEval(args, env) match {
        case head :: SExpr(tail) :: Nil => Quote(SExpr(head.unQuote :: tail))
        case head :: Quote(SExpr(tail)) :: Nil => Quote(SExpr(head.unQuote :: tail))
        case head :: tail :: Nil => Pair(head.unQuote, tail.unQuote)
        case _ :: _ :: _ :: Nil => Error(Message.ERR_ARITY_MISMATCH(2, args.size))
        case _ => Error(Message.ERR_INTERNAL)
      }
    }),
    "car" -> Builtin({ (args, env) =>
      safeEval(args, env) match {
        case SExpr(head :: _) :: Nil => head
        case Pair(head, _) :: Nil => head
        case _ :: Nil => Error(Message.ERR_BAD_ARGS("pair", "list"))
        case _ => Error(Message.ERR_ARITY_MISMATCH(1, args.size))
      }
    }),
    "cdr" -> Builtin({ (args, env) =>
      safeEval(args, env) match {
        case SExpr(_ :: tail) :: Nil => SExpr(tail)
        case Pair(_, tail) :: Nil => tail
        case _ :: Nil => Error(Message.ERR_BAD_ARGS("pair", "list"))
        case _ => Error(Message.ERR_ARITY_MISMATCH(1, args.size))
      }
    }),
    "cond" -> Builtin({ (args, env) =>
      args.find {
        case SExpr(cond :: _) =>
          safeEval(cond, env) match {
            case False => false
            case _ => true
          }

        case _ => false
      } match {
        case None => SExpr(List.empty)
        case Some(SExpr(_ :: Nil)) => SExpr(List.empty)
        case Some(SExpr(_ :: exprs)) => exprs.map(safeEval(_, env)).last
        case Some(expr) => Error(Message.ERR_EVAL_EXPR(expr))
      }
    }),
    "+" -> Builtin({ (args, env) =>
      def aux(exprs: List[Expression]): Expression =
        exprs match {
          case Nil => Integer(0)
          case h :: rest =>
            (h, aux(rest)) match {
              case (Integer(a), Integer(b)) => Integer(a + b)
              case (Real(a), Real(b)) => Real(a + b)
              case (Real(a), Integer(b)) => Real(a + b.toDouble)
              case (Integer(a), Real(b)) => Real(a.toDouble + b)
              case (a, b) => Error(Message.ERR_INVALID_ADD(a, b))
            }
        }

      aux(safeEval(args, env))
    }),
    "equal?" -> Builtin({ (args, env) =>
      safeEval(args, env) match {
        case lhs :: rhs :: Nil => BooleanExpr(lhs.unQuote == rhs.unQuote)
        case _ => Error(Message.ERR_ARITY_MISMATCH(2, args.size))
      }
    }),
    "type/name" -> Builtin({ (args, env) =>
      safeEval(args, env) match {
        case Builtin(_) :: Nil => Str("builtin")
        case Error(_, _) :: Nil => Str("error")
        case False :: Nil => Str("boolean")
        case Identifier(_) :: Nil => Str("identifier")
        case Integer(_) :: Nil => Str("integer")
        case Lambda(_, _, _, _) :: Nil => Str("lambda")
        case Pair(_, _) :: Nil => Str("pair")
        case Quote(_) :: Nil => Str("quote")
        case Real(_) :: Nil => Str("real")
        case SExpr(_) :: Nil => Str("sexpr")
        case Str(_) :: Nil => Str("string")
        case True :: Nil => Str("boolean")
        case _ :: _ => Error(Message.ERR_ARITY_MISMATCH(1, args.size))
        case Nil => Error(Message.ERR_ARITY_MISMATCH(1, 0))
      }
    }),
    "error" -> Builtin({ (args, env) =>
      safeEval(args, env) match {
        case Str(msg) :: Nil => Error(msg)
        case Quote(Identifier(msg)) :: Nil => Error(msg)
        case Quote(Str(msg)) :: Nil => Error(msg)
        case expr => Error(Message.ERR_INVALID_ERROR(expr))
      }
    }),
    "lambda" -> Builtin({ (args, env) =>
      def aux(raw: List[Expression], body: Expression, delayed: Boolean): Expression = {
        val (args, errs) = raw.partition {
          case Identifier(_) => true
          case _ => false
        }

        val names = args.map {
          case Identifier(name) => name
          case _ => ""
        }

        val dups = names.groupBy(identity) collect {
          case (x, xs) if xs.size > 1 => x
        }

        if (dups.size > 0) Error(Message.ERR_LAMBDA_DUP_ARGS(dups.toList))
        else if (errs.size > 0) Error(Message.ERR_LAMBDA_NON_ID_ARG)
        else Lambda(names, body, env, delayed)
      }

      args match {
        case Identifier("lazy") :: SExpr(raw) :: body :: Nil => aux(raw, body, true)
        case SExpr(raw) :: body :: Nil => aux(raw, body, false)
        case _ => Error(Message.ERR_BAD_SYNTAX("lambda"))
      }
    })
  )

  def eval(code: String, env: Environment): (List[Expression], Environment) =
    eval(new Parser(new Tokenizer(code)).toList, env)

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
      case Right(True) => (True, env)
      case Right(False) => (False, env)
      case Right(Integer(value)) => (Integer(value), env)
      case Right(Real(value)) => (Real(value), env)
      case Right(Str(value)) => (Str(value), env)

      case Right(Quote(Identifier(name))) =>
        (Quote(Identifier(name)), env)
      case Right(Quote(value)) => (value, env)

      case Right(SExpr(Identifier("load") :: Str(path) :: Nil)) =>
        Try { Files.readAllBytes(Paths.get(path)) } match {
          case Success(bytes) =>
            val code = new String(bytes, Charset.defaultCharset())
            val (_, next) = Interpreter.eval(code, env)

            // XXX Check for errors
            (Quote(Identifier("ok")), next)

          case Failure(_) =>
            (Error(s"Missing file: $path"), env)
        }

      case Right(SExpr(Identifier("define") :: Identifier(name) :: value :: Nil)) =>
        define(name, value, env)
      case Right(SExpr(Identifier("define") :: _)) =>
        (Error(Message.ERR_BAD_SYNTAX("define")), env)

      case Right(Identifier(label)) =>
        (builtin.getOrElse(label, env.lookup(label)), env)

      case Right(SExpr(fn :: args)) => procCall(fn, args, env)
      case Right(SExpr(Nil)) => (Error(Message.ERR_LAMBDA_EMPTY_CALL), env)

      case Right(expr) => (Error(Message.ERR_EXPRESSION(expr)), env)
      case Left(err) => (Error(Message.ERR_SYNTAX(err)), env)
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
      case proc: Lambda =>
        if (!proc.validArity(args.size))
          (Error(Message.ERR_ARITY_MISMATCH(proc.args.size, args.size)), env)
        else if (proc.delayed)
          (safeEval(proc.body, proc.scope(args, proc.env, env)), env)
        else
          // XXX Check that all arguments were evaluated
          (safeEval(proc.body, proc.scope(safeEval(args, env), proc.env, env)), env)

      case Builtin(fn) => (fn(args, env), env)
      case err: Error => (err, env)

      case _ =>
        (
          Error(
            Message.ERR_LAMBDA_NON_PROC_CALL,
            Some(Error(Message.GIVEN(fn.toString)))),
          env)
    }
}
