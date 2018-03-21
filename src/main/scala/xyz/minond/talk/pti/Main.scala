package xyz.minond.talk.pti

object Main {
  def main(args: Array[String]): Unit =
    new Parser(new Tokenizer("""

(define TRUE #t)

(define             one                 1)

(

define
two
2

)

(+ 1 2)

(define x*2
  (lambda (x)
    (* x 2)))

      """)) foreach println
}
