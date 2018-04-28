package xyz.minond.pti

import java.io.BufferedReader

object Repl {
  val welcome = "Welcome to PTI <https://github.com/minond/parse-to-interpretation>"
  val coreFile = """(load "lang/core.pti")"""
  val testFile = """(load "lang/test.pti")"""

  def run(reader: BufferedReader): Unit = {
    def aux(env: Environment, prefix: String): Unit = {
      if (prefix == "") print("> ")
      else print("  ")

      (prefix + " " + reader.readLine).trim match {
        case ".quit" => return
        case ".test" => aux(run(testFile, env), "")
        case ".reset" => aux(run(coreFile), "")
        case text =>
          if (!balanced(text)) aux(env, text)
          else aux(run(text, env), "")
      }
    }

    println(welcome)
    aux(run(coreFile), "")
  }

  def run(
      code: String,
      env: Environment = Builtins.load(Environment(Map()))): Environment =
    new Parser(new Tokenizer(code)).toList.foldLeft(env) { (env, expr) =>
      val (ret, next) = Interpreter.eval(expr, env)

      (expr, ret) match {
        case (_, err: Error) => show(err)
        case (Right(SExpr(Identifier("define") :: _)), _) =>
        case _ => show(ret)
      }

      next
    }

  def show(ret: Expression): Unit =
    ret match {
      case err: Error => println(err.stringify())
      case Quote(_, PrintfNl) => println("")
      case Quote(_, Internal) => print("")
      case res => println(res)
    }

  def balanced(src: String): Boolean =
    src.foldLeft(0)({
      case (sum, '(') => sum + 1
      case (sum, ')') => sum - 1
      case (sum, _) => sum
    }) == 0
}
