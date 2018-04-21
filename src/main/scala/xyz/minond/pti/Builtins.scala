package xyz.minond.pti

import scala.util.{Try, Failure}

object Builtins extends Loader {
  import Interpreter._

  def load(env: Environment): Environment =
    env
      .define(
        "eval",
        Procedure({ (args, env) =>
          eval(args, env) match {
            case expr :: Nil => eval(expr.unQuote, env)
            case Nil => Error(Message.ERR_ARITY_MISMATCH(1, 0))
            case exprs => Error(Message.ERR_ARITY_MISMATCH(1, exprs.size))
          }
        })
      )
      .define("unquote", Procedure({ (args, env) =>
        eval(args, env) match {
          case expr :: Nil => expr.unQuote
          case _ => Error(Message.ERR_ARITY_MISMATCH(1, args.size))
        }
      }))
      .define(
        "apply",
        Procedure({ (args, env) =>
          args match {
            case fn :: args =>
              procCall(
                fn,
                eval(args, env) match {
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
        })
      )
      .define(
        "cons",
        Procedure({ (args, env) =>
          eval(args, env) match {
            case head :: SExpr(tail) :: Nil => Quote(SExpr(head.unQuote :: tail))
            case head :: Quote(SExpr(tail), _) :: Nil =>
              Quote(SExpr(head.unQuote :: tail))
            case head :: tail :: Nil => Pair(head.unQuote, tail.unQuote)
            case _ :: _ :: _ :: Nil => Error(Message.ERR_ARITY_MISMATCH(2, args.size))
            case _ => Error(Message.ERR_INTERNAL)
          }
        })
      )
      .define(
        "car",
        Procedure({ (args, env) =>
          eval(args, env) match {
            case SExpr(head :: _) :: Nil => head
            case Pair(head, _) :: Nil => head
            case _ :: Nil => Error(Message.ERR_BAD_ARGS("car", "pair", "list"))
            case _ => Error(Message.ERR_ARITY_MISMATCH(1, args.size))
          }
        })
      )
      .define(
        "cdr",
        Procedure({ (args, env) =>
          eval(args, env) match {
            case SExpr(_ :: tail) :: Nil => SExpr(tail)
            case Pair(_, tail) :: Nil => tail
            case _ :: Nil => Error(Message.ERR_BAD_ARGS("cdr", "pair", "list"))
            case _ => Error(Message.ERR_ARITY_MISMATCH(1, args.size))
          }
        })
      )
      .define(
        "cond",
        Procedure({ (args, env) =>
          args.find {
            case SExpr(cond :: _) =>
              eval(cond, env) match {
                case False => false
                case _ => true
              }

            case _ => false
          } match {
            case None => SExpr(List.empty)
            case Some(SExpr(_ :: Nil)) => SExpr(List.empty)
            case Some(SExpr(_ :: exprs)) => exprs.map(eval(_, env)).last
            case Some(expr) => Error(Message.ERR_EVAL_EXPR(expr))
          }
        })
      )
      .define(
        "add",
        Procedure({ (args, env) =>
          eval(args, env) match {
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
        })
      )
      .define(
        "mult",
        Procedure({ (args, env) =>
          eval(args, env) match {
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
        })
      )
      .define(
        ">",
        Procedure({ (args, env) =>
          eval(args, env) match {
            case Integer(lhs) :: Integer(rhs) :: Nil => Bool(lhs > rhs)
            case Real(lhs) :: Real(rhs) :: Nil => Bool(lhs > rhs)
            case Real(lhs) :: Integer(rhs) :: Nil => Bool(lhs > rhs)
            case Integer(lhs) :: Real(rhs) :: Nil => Bool(lhs > rhs)
            case _ :: _ :: Nil => Error(Message.ERR_BAD_ARGS(">", "interger", "real"))
            case _ => Error(Message.ERR_ARITY_MISMATCH(2, args.size))
          }
        })
      )
      .define(
        "equal?",
        Procedure({ (args, env) =>
          eval(args, env) match {
            case lhs :: rhs :: Nil => Bool(lhs.unQuote == rhs.unQuote)
            case _ => Error(Message.ERR_ARITY_MISMATCH(2, args.size))
          }
        })
      )
      .define(
        "type/proc/arity",
        Procedure({ (args, env) =>
          eval(args, env) match {
            case Proc(args, _, _, _) :: Nil => Integer(args.size)
            case _ => Error(Message.ERR_BAD_ARGS("type/arity", "procedure"))
          }
        })
      )
      .define(
        "type/proc/vararg",
        Procedure({ (args, env) =>
          eval(args, env) match {
            case (proc: Proc) :: Nil => Bool(proc.isVariadic)
            case _ => Error(Message.ERR_BAD_ARGS("type/arity", "procedure"))
          }
        })
      )
      .define(
        "type/name",
        Procedure({ (args, env) =>
          eval(args, env) match {
            case Error(_, _) :: Nil => Identifier("error").quote
            case False :: Nil => Identifier("boolean").quote
            case Identifier(_) :: Nil => Identifier("identifier").quote
            case Integer(_) :: Nil => Identifier("integer").quote
            case Proc(_, _, _, _) :: Nil => Identifier("procedure").quote
            case Procedure(_) :: Nil => Identifier("procedure").quote
            case Pair(_, _) :: Nil => Identifier("pair").quote
            case Quote(_, _) :: Nil => Identifier("quote").quote
            case Real(_) :: Nil => Identifier("real").quote
            case SExpr(_) :: Nil => Identifier("sexpr").quote
            case Str(_) :: Nil => Identifier("string").quote
            case True :: Nil => Identifier("boolean").quote
            case _ :: _ => Error(Message.ERR_ARITY_MISMATCH(1, args.size))
            case Nil => Error(Message.ERR_ARITY_MISMATCH(1, 0))
          }
        })
      )
      .define("begin", Procedure({ (args, env) =>
        args
          .foldLeft[(Expression, Environment)]((ok(Internal), env)) {
            case ((_, env), expr) =>
              eval(Right(expr), env)
          }
          ._1
      }))
      .define("newline", Procedure({ (args, env) =>
        println("")
        ok(Internal)
      }))
      .define(
        "printf",
        Procedure({ (args, env) =>
          eval(args, env) match {
            case Str(fmt) :: args =>
              Try { printf(fmt, args: _*) } match {
                case Failure(ex) => Error(s"format error: ${ex.getMessage}")
                case _ => ok(PrintfNl)
              }

            case _ => Error(Message.ERR_BAD_ARGS("printf", "string"))
          }
        })
      )
      .define("exit", Procedure({ (args, env) =>
        System.exit(0)
        ok(Internal)
      }))
      .define(
        "halt",
        Procedure({ (args, env) =>
          throw new RuntimeException(eval(args, env) match {
            case Str(msg) :: Nil => msg
            case Quote(Identifier(msg), _) :: Nil => msg
            case Quote(Str(msg), _) :: Nil => msg
            case _ => "Halt"
          })
        })
      )
      .define(
        "error",
        Procedure({ (args, env) =>
          eval(args, env) match {
            case Str(msg) :: Nil => Error(msg)
            case Quote(Identifier(msg), _) :: Nil => Error(msg)
            case Quote(Str(msg), _) :: Nil => Error(msg)
            case expr => Error(Message.ERR_INVALID_ERROR(expr))
          }
        })
      )
      .define(
        "let*",
        Procedure({ (args, env) =>
          args match {
            case SExpr(defs) :: body :: Nil =>
              eval(body, defs.foldLeft(env) {
                case (env, SExpr(Identifier(name) :: body :: Nil)) =>
                  env.define(name, eval(body, env))

                case _ => env
              })

            case _ => Error(Message.ERR_BAD_SYNTAX("let*"))
          }
        })
      )
      .define(
        "lambda",
        Procedure({ (args, env) =>
          args match {
            case Identifier(":lazy") :: SExpr(raw) :: body :: Nil =>
              procDef(raw, body, env, true)
            case SExpr(raw) :: body :: Nil => procDef(raw, body, env)
            case _ => Error(Message.ERR_BAD_SYNTAX("lambda"))
          }
        })
      )
}
