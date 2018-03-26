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
      eval("0")._1 should be(List(IntNumberExpr(0)))
      eval("1")._1 should be(List(IntNumberExpr(1)))
      eval("123")._1 should be(List(IntNumberExpr(123)))
      eval("1 2 3")._1 should be(
        List(IntNumberExpr(1), IntNumberExpr(2), IntNumberExpr(3)))
      eval("""
      1
      2
      3""")._1 should be(List(IntNumberExpr(1), IntNumberExpr(2), IntNumberExpr(3)))
    }

    "evaluates real number expressions" in {
      eval("0.0")._1 should be(List(RealNumberExpr(0)))
      eval("1.1")._1 should be(List(RealNumberExpr(1.1)))
      eval("12.3")._1 should be(List(RealNumberExpr(12.3)))
      eval("1.0 2.0 3.0")._1 should be(
        List(RealNumberExpr(1.0), RealNumberExpr(2.0), RealNumberExpr(3.0)))
      eval("""
      .01
      .02
      .03""")._1 should be(
        List(RealNumberExpr(0.01), RealNumberExpr(0.02), RealNumberExpr(0.03)))
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
      eval(""""hi"""")._1 should be(List(StringExpr("hi")))
      eval(""""1""2""3"""")._1 should be(
        List(StringExpr("1"), StringExpr("2"), StringExpr("3")))
      eval(""""1" "2" "3"""")._1 should be(
        List(StringExpr("1"), StringExpr("2"), StringExpr("3")))
    }

    "evaluates quoted expressions" in {
      eval("'abc")._1 should be(List(QuoteExpr(IdentifierExpr("abc"))))
      eval("'123")._1 should be(List(IntNumberExpr(123)))
      eval("'123.456")._1 should be(List(RealNumberExpr(123.456)))
      eval("'#t")._1 should be(List(True))
      eval("'#f")._1 should be(List(False))
      eval("'(1 2 3)")._1 should be(
        List(SExpr(List(IntNumberExpr(1), IntNumberExpr(2), IntNumberExpr(3)))))
    }
  }

  "Builtin Function" - {
    "list creates a list" in {
      eval("(list 1 2 3)")._1 should be(
        List(SExpr(List(IntNumberExpr(1), IntNumberExpr(2), IntNumberExpr(3)))))
    }

    "error creates an error" in {
      eval("""(error "1 2 3")""")._1 should be(List(ErrorExpr("1 2 3")))
    }

    "eval evaluates scalar values" in {
      eval("(eval '123)")._1 should be(List(IntNumberExpr(123)))
      eval("(eval 123)")._1 should be(List(IntNumberExpr(123)))
      eval("(eval '123.456)")._1 should be(List(RealNumberExpr(123.456)))
      eval("(eval 123.456)")._1 should be(List(RealNumberExpr(123.456)))
      eval("(eval '#t)")._1 should be(List(True))
      eval("(eval #t)")._1 should be(List(True))
      eval("(eval '#f)")._1 should be(List(False))
      eval("(eval #f)")._1 should be(List(False))
      eval("""(eval '"hi")""")._1 should be(List(StringExpr("hi")))
      eval("""(eval "hi")""")._1 should be(List(StringExpr("hi")))
    }

    "eval evaluates s-expressions" in {
      eval("(eval '(+ 2 4))")._1 should be(List(IntNumberExpr(6)))
    }

    "cond does not evaluate expressions in false conditions" in {
      eval("""
        (cond
          (#f this is not ok)
          (#t 'ok))
      """)._1 should be(List(QuoteExpr(IdentifierExpr("ok"))))
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
  }
}
