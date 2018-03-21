package xyz.minond.talk.pti

object Main {
  def main(args: Array[String]): Unit =
    new Parser(new Tokenizer("(+ 1 2)")) foreach println
}
