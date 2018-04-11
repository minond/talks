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

    val ERR_REC_LOOKUP =
      "Detected recursive lookup. Haltin evaluation."
    def ERR_UNDEFINED_LOOKUP(label: String) =
      s"${label} is undefined."
    def ERR_ARITY_MISMATCH(expected: Int, got: Int) =
      s"Arity mismatch, expected ${expected} arguments but got ${got}."
    def ERR_BAD_ARGS(fn: String, args: String*) =
      s"Bad arguments passed to $fn proc, expecting ${args.mkString(", ")}."
    def ERR_INVALID_ERROR(exprs: List[Expression]) =
      s"Cannot use `(${exprs.mkString(" ")})` as an error message."

    val ERR_INTERNAL =
      "Internal error"
    def ERR_EVAL_EXPR(expr: Expression) =
      s"Error evaluating $expr."

    val ERR_PROC_EMPTY_CALL =
      "Missing procedure expression"
    val ERR_PROC_NON_PROC_CALL =
      "Call made to non-procedure."
    val ERR_PROC_NON_ID_ARG =
      "Found non-identifier argument(s) in lambda expression."
    def ERR_PROC_DUP_ARGS(dups: List[String]) =
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
    "apply" -> Builtin({ (args, env) =>
      args match {
        case fn :: args =>
          procCall(
            fn,
            safeEval(args, env) match {
              case head :: tail =>
                (head :: tail).lastOption match {
                  case Some(last) =>
                    last.unQuote match {
                      case SExpr(rest) => (head :: tail).init ++ rest
                      case _ => head :: tail
                    }

                  case None => List(head)
                }

              case Nil => List.empty
            },
            env
          )._1

        case _ => Error(Message.ERR_BAD_SYNTAX("apply"))
      }
    }),
    "cons" -> Builtin({ (args, env) =>
      safeEval(args, env) match {
        case head :: SExpr(tail) :: Nil => Quote(SExpr(head.unQuote :: tail))
        case head :: Quote(SExpr(tail), _) :: Nil => Quote(SExpr(head.unQuote :: tail))
        case head :: tail :: Nil => Pair(head.unQuote, tail.unQuote)
        case _ :: _ :: _ :: Nil => Error(Message.ERR_ARITY_MISMATCH(2, args.size))
        case _ => Error(Message.ERR_INTERNAL)
      }
    }),
    "car" -> Builtin({ (args, env) =>
      safeEval(args, env) match {
        case SExpr(head :: _) :: Nil => head
        case Pair(head, _) :: Nil => head
        case _ :: Nil => Error(Message.ERR_BAD_ARGS("car", "pair", "list"))
        case _ => Error(Message.ERR_ARITY_MISMATCH(1, args.size))
      }
    }),
    "cdr" -> Builtin({ (args, env) =>
      safeEval(args, env) match {
        case SExpr(_ :: tail) :: Nil => SExpr(tail)
        case Pair(_, tail) :: Nil => tail
        case _ :: Nil => Error(Message.ERR_BAD_ARGS("cdr", "pair", "list"))
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
    "add" -> Builtin({ (args, env) =>
      safeEval(args, env) match {
        case Nil => Integer(0)
        case Integer(a) :: Nil => Integer(a)
        case Real(a) :: Nil => Real(a)
        case Real(a) :: Real(b) :: Nil => Real(a + b)
        case Integer(a) :: Integer(b) :: Nil => Integer(a + b)
        case Integer(a) :: Real(b) :: Nil => Real(a.toDouble + b)
        case Real(a) :: Integer(b) :: Nil => Real(a + b.toDouble)
        case _ :: _ :: _ :: Nil => Error(Message.ERR_ARITY_MISMATCH(args.size, 2))
        case _ => Error(Message.ERR_BAD_ARGS("add", "real", "interger"))
      }
    }),
    "mult" -> Builtin({ (args, env) =>
      safeEval(args, env) match {
        case Nil => Integer(1)
        case Integer(a) :: Nil => Integer(a)
        case Real(a) :: Nil => Real(a)
        case Real(a) :: Real(b) :: Nil => Real(a * b)
        case Integer(a) :: Integer(b) :: Nil => Integer(a * b)
        case Integer(a) :: Real(b) :: Nil => Real(a.toDouble * b)
        case Real(a) :: Integer(b) :: Nil => Real(a * b.toDouble)
        case _ :: _ :: _ :: Nil => Error(Message.ERR_ARITY_MISMATCH(args.size, 2))
        case _ => Error(Message.ERR_BAD_ARGS("mult", "real", "interger"))
      }
    }),
    ">" -> Builtin({ (args, env) =>
      safeEval(args, env) match {
        case Integer(lhs) :: Integer(rhs) :: Nil => Bool(lhs > rhs)
        case Real(lhs) :: Real(rhs) :: Nil => Bool(lhs > rhs)
        case Real(lhs) :: Integer(rhs) :: Nil => Bool(lhs > rhs)
        case Integer(lhs) :: Real(rhs) :: Nil => Bool(lhs > rhs)
        case _ :: _ :: Nil => Error(Message.ERR_BAD_ARGS(">", "interger", "real"))
        case _ => Error(Message.ERR_ARITY_MISMATCH(2, args.size))
      }
    }),
    "equal?" -> Builtin({ (args, env) =>
      safeEval(args, env) match {
        case lhs :: rhs :: Nil => Bool(lhs.unQuote == rhs.unQuote)
        case _ => Error(Message.ERR_ARITY_MISMATCH(2, args.size))
      }
    }),
    "type/proc/arity" -> Builtin({ (args, env) =>
      safeEval(args, env) match {
        case Proc(args, _, _, _) :: Nil => Integer(args.size)
        case _ => Error(Message.ERR_BAD_ARGS("type/arity", "procedure"))
      }
    }),
    "type/proc/vararg" -> Builtin({ (args, env) =>
      safeEval(args, env) match {
        case (proc: Proc) :: Nil => Bool(proc.isVariadic)
        case _ => Error(Message.ERR_BAD_ARGS("type/arity", "procedure"))
      }
    }),
    "type/name" -> Builtin({ (args, env) =>
      safeEval(args, env) match {
        case Builtin(_) :: Nil => Identifier("builtin").quote
        case Error(_, _) :: Nil => Identifier("error").quote
        case False :: Nil => Identifier("boolean").quote
        case Identifier(_) :: Nil => Identifier("identifier").quote
        case Integer(_) :: Nil => Identifier("integer").quote
        case Proc(_, _, _, _) :: Nil => Identifier("procedure").quote
        case Pair(_, _) :: Nil => Identifier("pair").quote
        case Quote(_, _) :: Nil => Identifier("quote").quote
        case Real(_) :: Nil => Identifier("real").quote
        case SExpr(_) :: Nil => Identifier("sexpr").quote
        case Str(_) :: Nil => Identifier("string").quote
        case True :: Nil => Identifier("boolean").quote
        case _ :: _ => Error(Message.ERR_ARITY_MISMATCH(1, args.size))
        case Nil => Error(Message.ERR_ARITY_MISMATCH(1, 0))
      }
    }),
    "begin" -> Builtin({ (args, env) =>
      args
        .foldLeft[(Expression, Environment)]((ok(Internal), env)) {
          case ((_, env), expr) =>
            eval(Right(expr), env)
        }
        ._1
    }),
    "newline" -> Builtin({ (args, env) =>
      println("")
      ok(Internal)
    }),
    "printf" -> Builtin({ (args, env) =>
      safeEval(args, env) match {
        case Str(fmt) :: args =>
          Try { printf(fmt, args: _*) } match {
            case Failure(ex) => Error(s"format error: ${ex.getMessage}")
            case _ => ok(PrintfNl)
          }

        case _ => Error(Message.ERR_BAD_ARGS("printf", "string"))
      }
    }),
    "exit" -> Builtin({ (args, env) =>
      System.exit(0)
      ok(Internal)
    }),
    "halt" -> Builtin({ (args, env) =>
      throw new RuntimeException(safeEval(args, env) match {
        case Str(msg) :: Nil => msg
        case Quote(Identifier(msg), _) :: Nil => msg
        case Quote(Str(msg), _) :: Nil => msg
        case _ => "Halt"
      })
    }),
    "error" -> Builtin({ (args, env) =>
      safeEval(args, env) match {
        case Str(msg) :: Nil => Error(msg)
        case Quote(Identifier(msg), _) :: Nil => Error(msg)
        case Quote(Str(msg), _) :: Nil => Error(msg)
        case expr => Error(Message.ERR_INVALID_ERROR(expr))
      }
    }),
    "let*" -> Builtin({ (args, env) =>
      args match {
        case SExpr(defs) :: body :: Nil =>
          safeEval(body, defs.foldLeft(env) {
            case (env, SExpr(Identifier(name) :: body :: Nil)) =>
              env.define(name, safeEval(body, env))

            case _ => env
          })

        case _ => Error(Message.ERR_BAD_SYNTAX("let*"))
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

        if (dups.size > 0) Error(Message.ERR_PROC_DUP_ARGS(dups.toList))
        else if (errs.size > 0) Error(Message.ERR_PROC_NON_ID_ARG)
        else Proc(names, body, env, delayed)
      }

      args match {
        case Identifier(":lazy") :: SExpr(raw) :: body :: Nil => aux(raw, body, true)
        case SExpr(raw) :: body :: Nil => aux(raw, body, false)
        case _ => Error(Message.ERR_BAD_SYNTAX("lambda"))
      }
    })
  )

  def eval(code: String): (List[Expression], Environment) =
    eval(new Parser(new Tokenizer(code)).toList, Environment(Map()))

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

      case Right(Quote(Identifier(name), _)) =>
        (Quote(Identifier(name)), env)
      case Right(Quote(value, _)) => (value, env)

      case Right(SExpr(Identifier("load") :: Str(path) :: Nil)) =>
        Try { Files.readAllBytes(Paths.get(path)) } match {
          case Success(bytes) =>
            val code = new String(bytes, Charset.defaultCharset())
            val (_, next) = Interpreter.eval(code, env)

            // XXX Check for errors
            (ok(Internal), next)

          case Failure(_) =>
            (Error(s"Missing file: $path"), env)
        }

      case Right(SExpr(Identifier("define") :: Identifier(name) :: value :: Nil)) =>
        define(name, value, env)
      case Right(SExpr(Identifier("define") :: _)) =>
        (Error(Message.ERR_BAD_SYNTAX("define")), env)

      case Right(Identifier(label)) =>
        try {
          (builtin.getOrElse(label, env.lookup(label)), env)
        } catch {
          case _: StackOverflowError => (Error(Message.ERR_REC_LOOKUP), env)
          case err: Exception => (Error(err.getMessage), env)
        }

      case Right(SExpr(fn :: args)) => procCall(fn, args, env)
      case Right(SExpr(Nil)) => (Error(Message.ERR_PROC_EMPTY_CALL), env)

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
      case proc: Proc =>
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
          Error(Message.ERR_PROC_NON_PROC_CALL, Some(Error(Message.GIVEN(fn.toString)))),
          env)
    }

  def ok(info: QuoteInfo = UserSpace): Expression =
    Quote(Identifier("ok"), info)
}
