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


(lambda (x) (* x 2))

(define x 1)
(define y 2)

(define z
  (lambda (n)
    (* n n)))

(define a x)

(define x x)

x
y
z

    """

    val (vals, env) =
      Interpreter.eval(new Parser(new Tokenizer(source)).toList, Environment(Map()))

    vals foreach println
    println()
    println(env)
  }
}
