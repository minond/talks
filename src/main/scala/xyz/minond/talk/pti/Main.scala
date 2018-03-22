package xyz.minond.talk.pti

object Main {
  def main(args: Array[String]): Unit = {
    val source = """

(+ 1 2 3 4)


(error "Error: 1 2 3")


'(1 2 3)

(list 1 2 3)

(equal? 1 1)
(equal? '(1 2 3) '(1 2 3))
(equal? '(1 2 3) '(3 2 1))

    """

    new Parser(new Tokenizer(source)) foreach { expr =>
      println(Interpreter.eval(expr))
    }
  }
}
