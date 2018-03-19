import org.scalatest._

import xyz.minond.talk.pti.Scanner
import xyz.minond.talk.pti.Token
import xyz.minond.talk.pti.Token._

class ExampleSpec extends FlatSpec with Matchers {
  def scan(src: String) =
    new Scanner(src) toList

  "The Scanner" should "handle empty input" in {
    scan("") should be(List())
    scan(" ") should be(List())
    scan("          ") should be(List())
  }

  it should "tokenize identifiers" in {
    scan("name") should be(List(Token(IDENTIFIER, Some("name"))))
    scan("dot.dot") should be(List(Token(IDENTIFIER, Some("dot.dot"))))
    scan("dash-dash") should be(List(Token(IDENTIFIER, Some("dash-dash"))))
    scan("obj->obj") should be(List(Token(IDENTIFIER, Some("obj->obj"))))
  }

  it should "tokenize strings" in {
    val twostrs = List(
      Token(STRING, Some("1 2 3")),
      Token(STRING, Some("4 5 6"))
    )

    scan(""""1 2 3"""") should be(List(Token(STRING, Some("1 2 3"))))
    scan(""""1 2 3""4 5 6"""") should be(twostrs)
    scan(""""1 2 3"   "4 5 6"""") should be(twostrs)
  }

  it should "tokenize numbers" in {
    scan("1") should be(List(Token(NUMBER, Some("1"))))
    scan("123") should be(List(Token(NUMBER, Some("123"))))
    scan("0123456789") should be(List(Token(NUMBER, Some("0123456789"))))
    scan("9876543210") should be(List(Token(NUMBER, Some("9876543210"))))
  }

  it should "tokenize parentheses" in {
    scan("()") should be(List(Token(OPEN_PAREN), Token(CLOSE_PAREN)))
    scan("((()))") should be(
      List(Token(OPEN_PAREN),
           Token(OPEN_PAREN),
           Token(OPEN_PAREN),
           Token(CLOSE_PAREN),
           Token(CLOSE_PAREN),
           Token(CLOSE_PAREN)))
  }

  it should "tokenize content inside of parentheses" in {
    scan("(123)") should be(
      List(Token(OPEN_PAREN), Token(NUMBER, Some("123")), Token(CLOSE_PAREN)))

    scan("(((123)))") should be(
      List(Token(OPEN_PAREN),
           Token(OPEN_PAREN),
           Token(OPEN_PAREN),
           Token(NUMBER, Some("123")),
           Token(CLOSE_PAREN),
           Token(CLOSE_PAREN),
           Token(CLOSE_PAREN)))
  }

  it should "tokenize content separated by spaces that is inside of parentheses" in {
    scan("(+ 1 2)") should be(
      List(Token(OPEN_PAREN),
           Token(IDENTIFIER, Some("+")),
           Token(NUMBER, Some("1")),
           Token(NUMBER, Some("2")),
           Token(CLOSE_PAREN)))

    scan("(((+ 1 2)))") should be(
      List(
        Token(OPEN_PAREN),
        Token(OPEN_PAREN),
        Token(OPEN_PAREN),
        Token(IDENTIFIER, Some("+")),
        Token(NUMBER, Some("1")),
        Token(NUMBER, Some("2")),
        Token(CLOSE_PAREN),
        Token(CLOSE_PAREN),
        Token(CLOSE_PAREN)
      ))

    scan("(((+ 1 (* 2 3))))") should be(
      List(
        Token(OPEN_PAREN),
        Token(OPEN_PAREN),
        Token(OPEN_PAREN),
        Token(IDENTIFIER, Some("+")),
        Token(NUMBER, Some("1")),
        Token(OPEN_PAREN),
        Token(IDENTIFIER, Some("*")),
        Token(NUMBER, Some("2")),
        Token(NUMBER, Some("3")),
        Token(CLOSE_PAREN),
        Token(CLOSE_PAREN),
        Token(CLOSE_PAREN),
        Token(CLOSE_PAREN)
      ))
  }
}
