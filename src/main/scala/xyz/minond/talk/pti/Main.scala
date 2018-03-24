package xyz.minond.talk.pti

import java.io.{BufferedReader, InputStreamReader}

object Main {
  def main(args: Array[String]): Unit =
    repl

  def repl: Unit = {
    val input = new InputStreamReader(System.in)
    val reader = new BufferedReader(input)

    def aux(env: Environment): Unit = {
      print("> ")

      reader.readLine match {
        case "(exit)" => return
        case text =>
          val src = new Parser(new Tokenizer(text))
          val (vals, next) = Interpreter.eval(src.toList, env)
          vals foreach println
          aux(next)
      }
    }

    aux(Environment(Map()))
  }
}
