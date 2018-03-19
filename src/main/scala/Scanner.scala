package xyz.minond.talk.pti

import Token._
import scala.util.{Try, Success, Failure}

class Scanner(raw: String) extends Iterator[Try[Token]] {
  val src = raw.trim.toList.toIterator.buffered

  def ok(id: Token.Id, lexeme: Option[String] = None) =
    Success(Token(id, lexeme))

  def next(): Try[Token] = {
    src.next match {
      case ' '  => next
      case '('  => ok(OPEN_PAREN)
      case ')'  => ok(CLOSE_PAREN)
      case '#'  => ok(POUND)
      case '\'' => ok(SQUOTE)

      case '"' =>
        val str = Some(lookbehind((curr, prev) =>
          curr != '"' || prev == Some('\\')).mkString)

        (src.hasNext, if (src.hasNext) src.head else 0) match {
          case (true, '"') =>
            src.next
            ok(STRING, str)

          // Internal error, string did not end with '"' but still has more
          // input for some reason. This shouldn't happen.
          case (true, _) =>
            src.next
            ok(INVALID, str)

          // User error, string did not end with a '"' character
          case (false, _) =>
            ok(INVALID, str)
        }

      case n if isDigit(n) =>
        val digits = n :: consume(or(isDigit, is('.'))).toList
        val num = Some(digits.mkString)

        digits.count(is('.')) match {
          case 0 => ok(INTEGER, num)
          case 1 => ok(REAL, num)
          case _ => ok(INVALID, num)
        }

      case x =>
        val chars = x :: consume(isIdentifier(_: Char)).toList
        ok(IDENTIFIER, Some(chars.mkString))
    }
  }

  // XXX Clean this function up
  def lookbehind(f: (Char, Option[Char]) => Boolean) = {
    var buff = List[Char]()
    var prev: Option[Char] = None

    while (src.hasNext && f(src.head, prev)) {
      buff = buff ++ List(src.head)
      prev = Some(src.head)
      src.next
    }

    buff
  }

  def consume(f: Char => Boolean) =
    lookbehind((x: Char, _) => f(x))

  def hasNext(): Boolean =
    src.hasNext

  def isDigit(c: Char): Boolean =
    c >= '0' && c <= '9'

  def isIdentifier(c: Char): Boolean =
    c != '(' && c != ')' && c != ' '

  def is(c: Char) =
    (x: Char) => c == x

  def not(f: Char => Boolean) =
    (x: Char) => !f(x)

  def or(f1: Char => Boolean, f2: (Char) => Boolean) =
    (x: Char) => f1(x) || f2(x)
}
