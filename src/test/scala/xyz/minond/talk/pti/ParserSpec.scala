package xyz.minond.talk.pti

import Statement._
import org.scalatest._

class ParserSpec extends FlatSpec with Matchers {
  def parse(src: String) =
    new Parser(new Tokenizer(src)).toList collect {
      case Right(stmt) => stmt
      case Left(err)   => throw new Exception(err.message)
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
    parse("#f") should be(List(BooleanStmt(false)))
    parse("#t") should be(List(BooleanStmt(true)))
  }

  it should "fail to parse an invalid boolea value" in {
    a[Exception] should be thrownBy {
      parse("#123")
    }
  }
}
