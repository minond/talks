package xyz.minond.talk.pti

import org.scalatest._

class InterpreterSpec extends FreeSpec with Matchers {
  def eval(src: String) =
    Interpreter.eval(new Parser(new Tokenizer(src)).toList, Environment(Map()))

  "The Interpreter" - {
    "handles empty input" in {
      eval("")._1 should be(List())
      eval(" ")._1 should be(List())
      eval("          ")._1 should be(List())
      eval("				")._1 should be(List())
      eval("""


  """)._1 should be(List())
    }

    "evaluates integer expressions" in {
      eval("0")._1 should be(List(Integer(0)))
      eval("1")._1 should be(List(Integer(1)))
      eval("123")._1 should be(List(Integer(123)))
      eval("1 2 3")._1 should be(List(Integer(1), Integer(2), Integer(3)))
      eval("""
      1
      2
      3""")._1 should be(List(Integer(1), Integer(2), Integer(3)))
    }

    "evaluates real number expressions" in {
      eval("0.0")._1 should be(List(Real(0)))
      eval("1.1")._1 should be(List(Real(1.1)))
      eval("12.3")._1 should be(List(Real(12.3)))
      eval("1.0 2.0 3.0")._1 should be(List(Real(1.0), Real(2.0), Real(3.0)))
      eval("""
      .01
      .02
      .03""")._1 should be(List(Real(0.01), Real(0.02), Real(0.03)))
    }

    "evaluates boolean expressions" in {
      eval("#t")._1 should be(List(True))
      eval("#f")._1 should be(List(False))
      eval("#t #f #t #f")._1 should be(List(True, False, True, False))
      eval("""
      #f
      #t
      #f
      #t""")._1 should be(List(False, True, False, True))
    }

    "evaluates string expressions" in {
      eval(""""hi"""")._1 should be(List(Str("hi")))
      eval(""""1""2""3"""")._1 should be(List(Str("1"), Str("2"), Str("3")))
      eval(""""1" "2" "3"""")._1 should be(List(Str("1"), Str("2"), Str("3")))
    }

    "evaluates quoted expressions" in {
      eval("'abc")._1 should be(List(Quote(Identifier("abc"))))
      eval("'123")._1 should be(List(Integer(123)))
      eval("'123.456")._1 should be(List(Real(123.456)))
      eval("'#t")._1 should be(List(True))
      eval("'#f")._1 should be(List(False))
      eval("'(1 2 3)")._1 should be(List(SExpr(List(Integer(1), Integer(2), Integer(3)))))
    }
  }

  "Builtin Function" - {
    "error creates an error" in {
      eval("""(error "1 2 3")""")._1 should be(List(Error("1 2 3")))
    }

    "eval evaluates scalar values" in {
      eval("(eval '123)")._1 should be(List(Integer(123)))
      eval("(eval 123)")._1 should be(List(Integer(123)))
      eval("(eval '123.456)")._1 should be(List(Real(123.456)))
      eval("(eval 123.456)")._1 should be(List(Real(123.456)))
      eval("(eval '#t)")._1 should be(List(True))
      eval("(eval #t)")._1 should be(List(True))
      eval("(eval '#f)")._1 should be(List(False))
      eval("(eval #f)")._1 should be(List(False))
      eval("""(eval '"hi")""")._1 should be(List(Str("hi")))
      eval("""(eval "hi")""")._1 should be(List(Str("hi")))
    }

    "eval evaluates s-expressions" in {
      eval("(eval '(add 2 4))")._1 should be(List(Integer(6)))
    }

    "cond does not evaluate expressions in false conditions" in {
      eval("""
        (cond
          (#f this is not ok)
          (#t 'ok))
      """)._1 should be(List(Quote(Identifier("ok"))))
    }

    "cond does not evaluate exressions in false conditions" in {
      eval("""
        (cond
          (#f 'err)
          (#f 'err))
      """)._1 should be(List(SExpr(List.empty)))
    }

    "cond runs in its own scope" in {
      eval("""
        (cond
          (#f (define a 1))
          (#f (define b 2))
          (#t (define c 3)))
      """)._2 should be(Environment(Map()))
    }

    "let* does not leak definitions" in {
      eval("""
        (let* ((x 1))
          2)
      """)._2 should be(Environment(Map()))
    }

    "let* allows access to definitions" in {
      eval("""
        (let* ((x 2))
          x)
      """)._1 should be(List(Integer(2)))

      eval("""
        (let* ((x 2))
          (mult x x))
      """)._1 should be(List(Integer(4)))
    }

    "let* allows access to definitions in other definitions" in {
      eval("""
        (let* ((x 2)
               (y x))
          y)
      """)._1 should be(List(Integer(2)))

      eval("""
        (let* ((x 2)
               (y (mult x x)))
          (add y 1))
      """)._1 should be(List(Integer(5)))
    }
  }
}
