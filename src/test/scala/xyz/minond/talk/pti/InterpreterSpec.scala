package xyz.minond.talk.pti

import org.scalatest._

class InterpreterSpec extends FlatSpec with Matchers {
  def eval(src: String) =
    Interpreter.eval(new Parser(new Tokenizer(src)).toList, Environment(Map()))

  "The Interpreter" should "handle empty input" in {
    eval("")._1 should be(List())
    eval(" ")._1 should be(List())
    eval("          ")._1 should be(List())
    eval("				")._1 should be(List())
    eval("""


    """)._1 should be(List())
  }

  it should "evaluate integer expressions" in {
    eval("0")._1 should be(List(IntNumberValue(0)))
    eval("1")._1 should be(List(IntNumberValue(1)))
    eval("123")._1 should be(List(IntNumberValue(123)))
    eval("1 2 3")._1 should be(
      List(IntNumberValue(1), IntNumberValue(2), IntNumberValue(3)))
    eval("""
      1
      2
      3""")._1 should be(List(IntNumberValue(1), IntNumberValue(2), IntNumberValue(3)))
  }

  it should "evaluate real number expressions" in {
    eval("0.0")._1 should be(List(RealNumberValue(0)))
    eval("1.1")._1 should be(List(RealNumberValue(1.1)))
    eval("12.3")._1 should be(List(RealNumberValue(12.3)))
    eval("1.0 2.0 3.0")._1 should be(
      List(RealNumberValue(1.0), RealNumberValue(2.0), RealNumberValue(3.0)))
    eval("""
      .01
      .02
      .03""")._1 should be(
      List(RealNumberValue(0.01), RealNumberValue(0.02), RealNumberValue(0.03)))
  }

  it should "evaluate boolean expressions" in {
    eval("#t")._1 should be(List(BooleanValue(true)))
    eval("#f")._1 should be(List(BooleanValue(false)))
    eval("#t #f #t #f")._1 should be(
      List(
        BooleanValue(true),
        BooleanValue(false),
        BooleanValue(true),
        BooleanValue(false)))
    eval("""
      #f
      #t
      #f
      #t""")._1 should be(
      List(
        BooleanValue(false),
        BooleanValue(true),
        BooleanValue(false),
        BooleanValue(true)
      ))
  }

  it should "evaluate string expressions" in {
    eval(""""hi"""")._1 should be(List(StringValue("hi")))
    eval(""""1""2""3"""")._1 should be(
      List(StringValue("1"), StringValue("2"), StringValue("3")))
    eval(""""1" "2" "3"""")._1 should be(
      List(StringValue("1"), StringValue("2"), StringValue("3")))
  }

  it should "evaluate lists" in {
    eval("(list 1 2 3)")._1 should be(
      List(ListValue(List(IntNumberValue(1), IntNumberValue(2), IntNumberValue(3)))))
    eval("'(1 2 3)")._1 should be(
      List(ListValue(List(IntNumberValue(1), IntNumberValue(2), IntNumberValue(3)))))
  }

  it should "evaluate erros" in {
    eval("""(error "1 2 3")""")._1 should be(List(ErrorValue("1 2 3")))
  }

  it should "evaluate quoted expressions" in {
    eval("'abc")._1 should be(List(SymbolValue("abc")))
    eval("'123")._1 should be(List(IntNumberValue(123)))
    eval("'123.456")._1 should be(List(RealNumberValue(123.456)))
    eval("'#t")._1 should be(List(BooleanValue(true)))
    eval("'#f")._1 should be(List(BooleanValue(false)))
  }
}
