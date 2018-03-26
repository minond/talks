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
          if (!balanced(text))
            aux(env, text)
          else {
            val src = new Parser(new Tokenizer(text))
            val (vals, next) = Interpreter.eval(src.toList, env)

            vals foreach {
              case err: Error => println(err.stringify())
              case res => println(res)
            }

            aux(next, "")
          }
      }
    }

    aux(Environment(Map()), "")
  }

  def balanced(src: String): Boolean =
    src.foldLeft(0)({
      case (sum, '(') => sum + 1
      case (sum, ')') => sum - 1
      case (sum, _) => sum
    }) == 0
}
