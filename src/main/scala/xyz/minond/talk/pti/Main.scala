package xyz.minond.talk.pti

object Main {
  def main(args: Array[String]): Unit = {
    var source = """

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
    (+ n n)))

(define a x)

(define x x)

x
y
z

(z 1 2 3)
(z (+ 1 (+ 2 3)))

((lambda (x) (+ 2 x)) 21)

(define add-x
  (lambda (x)
    (lambda (n)
      (+ n x))))

(define add-10 (add-x 10))
(add-10 13)

    """

    source = """
(define add-x
  (lambda (x)
    (lambda (n)
      (+ n x))))

(define add-10 (add-x 10))
(add-10 13)

(define add-y
  (lambda ()
    (lambda (n)
      (+ n y))))

(define add-what-ever-y-is (add-y))
(define y 13)
(add-what-ever-y-is 12)
    """

    source = """

(define scope-test
  (lambda (a)
    (lambda (b)
      (lambda (c)
        (lambda (d)
          (lambda (e)
            (lambda (f)
              (lambda (g)
                (+ a b c d e f g)))))))))

((((((scope-test 2) 3) 4) 5) 6) 7)
(((((((scope-test 2) 3) 4) 5) 6) 7) 8)

    """

    source = """

(cond
  ('12 '3)
  ('a 'b)
  (#t 123)
  (#f 321))

    """

    source = """

(define not
  (lambda (x)
    (cond
      (x #f)
      (#t #t))))

(define and
  (lambda (a b)
    (cond
      (a b)
      (#t #f))))

(define or
  (lambda (a b)
    (cond
      (a #t)
      (#t b))))

(define null?
  (lambda (xs)
    (equal? xs '())))

(not #t)
(not #f)

    """

    val (vals, env) =
      Interpreter.eval(new Parser(new Tokenizer(source)).toList, Environment(Map()))

    vals foreach println
    println()
    println(env)
  }
}
