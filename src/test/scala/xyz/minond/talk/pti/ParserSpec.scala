package xyz.minond.talk.pti

import org.scalatest._

class ParserSpec extends FlatSpec with Matchers {
  def parse(src: String) =
    new Parser(new Tokenizer(src)).toList collect {
      case Right(expr) => expr
      case Left(err) => throw new Exception(err.message)
    }

  "The Parser" should "handle empty input" in {
    parse("") should be(List())
    parse(" ") should be(List())
    parse("          ") should be(List())
    parse("				") should be(List())
    parse("""


    """) should be(List())
  }

  it should "parse valid boolean values" in {
    parse("#f") should be(List(BooleanExpr(false)))
    parse("#t") should be(List(BooleanExpr(true)))
  }

  it should "fail to parse an invalid boolean value" in {
    a[Exception] should be thrownBy {
      parse("#123")
    }
  }

  it should "parse valid integer numbers" in {
    parse("1") should be(List(IntNumberExpr(1)))
    parse("123") should be(List(IntNumberExpr(123)))
    parse("1 2 3") should be(
      List(
        IntNumberExpr(1),
        IntNumberExpr(2),
        IntNumberExpr(3)
      ))
  }

  it should "parse valid real numbers" in {
    parse("1.0") should be(List(RealNumberExpr(1.0)))
    parse("1.23") should be(List(RealNumberExpr(1.23)))
    parse("0.123") should be(List(RealNumberExpr(0.123)))
    parse("0.1 0.2 0.3") should be(
      List(
        RealNumberExpr(0.1),
        RealNumberExpr(0.2),
        RealNumberExpr(0.3)
      ))
  }

  it should "parse valid identifiers" in {
    parse("+") should be(List(IdentifierExpr("+")))
    parse("=") should be(List(IdentifierExpr("=")))
    parse("mod") should be(List(IdentifierExpr("mod")))
    parse("number->string") should be(List(IdentifierExpr("number->string")))
    parse("package.name") should be(List(IdentifierExpr("package.name")))
  }

  it should "parse valid s-expressions" in {
    parse("()") should be(List(SExpr(List())))
    parse("(())") should be(List(SExpr(List(SExpr(List())))))
    parse("((()))") should be(List(SExpr(List(SExpr(List(SExpr(List())))))))

    parse("((() () ()))") should be(
      List(
        SExpr(
          List(
            SExpr(
              List(
                SExpr(List()),
                SExpr(List()),
                SExpr(List()),
              ))))))

    parse("(+ 1 2)") should be(
      List(SExpr(List(IdentifierExpr("+"), IntNumberExpr(1), IntNumberExpr(2)))))
  }

  it should "fail to parse an s-expressions with invalid contents" in {
    a[Exception] should be thrownBy {
      parse("(#invalid)")
    }
  }

  it should "parse valid quoted expressions" in {
    parse("'a") should be(List(QuoteExpr(IdentifierExpr("a"))))
    parse("'abc") should be(List(QuoteExpr(IdentifierExpr("abc"))))
    parse("'1") should be(List(QuoteExpr(IntNumberExpr(1))))
    parse("'123") should be(List(QuoteExpr(IntNumberExpr(123))))
    parse("'()") should be(List(QuoteExpr(SExpr(List()))))
    parse("'#f") should be(List(QuoteExpr(BooleanExpr(false))))
  }

  it should "fail to parse invalid quoted expressions" in {
    a[Exception] should be thrownBy {
      parse("'")
    }

    a[Exception] should be thrownBy {
      parse("'#x")
    }
  }

  it should "parse valid strings" in {
    parse(""""hi"""") should be(List(StringExpr("hi")))
    parse(""""1 2 3"""") should be(List(StringExpr("1 2 3")))
    parse(""""1""2""3"""") should be(
      List(
        StringExpr("1"),
        StringExpr("2"),
        StringExpr("3")
      ))

    parse(""""1" "2" "3"""") should be(
      List(
        StringExpr("1"),
        StringExpr("2"),
        StringExpr("3")
      ))
  }
}
