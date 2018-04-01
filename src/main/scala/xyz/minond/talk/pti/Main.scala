package xyz.minond.talk.pti

import java.io.{BufferedReader, InputStreamReader}

object Main {
  def main(args: Array[String]): Unit =
    repl

  def repl: Unit = {
    val input = new InputStreamReader(System.in)
    val reader = new BufferedReader(input)

    def aux(env: Environment, prefix: String): Unit = {
      if (prefix == "") print("> ")
      else print("  ")

      (prefix + " " + reader.readLine).trim match {
        case "(exit)" => return
        case text =>
          if (!balanced(text)) aux(env, text)
          else aux(run(text, env), "")
      }
    }

    println("Welcome to PTI <https://github.com/minond/talk-parse-to-interpretation>")
    print("Loading core... ")
    aux(run("""(load "src/lang/core.rkt")"""), "")
  }

  def run(code: String, env: Environment = Environment(Map())): Environment = {
    val (vals, next) = Interpreter.eval(code, env)
    show(vals)
    next
  }

  def show(vals: List[Expression]): Unit =
    vals foreach {
      case err: Error => println(err.stringify())
      case res => println(res)
    }

  def balanced(src: String): Boolean =
    src.foldLeft(0)({
      case (sum, '(') => sum + 1
      case (sum, ')') => sum - 1
      case (sum, _) => sum
    }) == 0
}
