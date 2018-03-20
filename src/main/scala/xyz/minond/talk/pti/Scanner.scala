package xyz.minond.talk.pti

import Token._

object Scanner {
  object Error {
    val STR_NO_CLOSING_WITH_CONT = """String did not end with a '"' but there is still input."""
    val STR_NO_CLOSING = """String did not end with a '"' character."""
    val NUM_MULT_PERIOUS = "Found multiple periods in number."
  }
}

class Scanner(raw: String) extends Iterator[Either[Error, Token]] {
  val src = raw.trim.toList.toIterator.buffered

  def ok(id: Token.Id, lexeme: Option[String] = None) =
    Right(Token(id, lexeme))

  def err(message: String, lexeme: Option[String] = None) =
    Left(Error(message, lexeme))

  def next() = {
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

          case (true, _) =>
            src.next
            err(Scanner.Error.STR_NO_CLOSING_WITH_CONT, str)

          case (false, _) =>
            err(Scanner.Error.STR_NO_CLOSING, str)
        }

      case n if isDigit(n) =>
        val digits = n :: consume(or(isDigit, is('.'))).toList
        val num = Some(digits.mkString)

        digits.count(is('.')) match {
          case 0 => ok(INTEGER, num)
          case 1 => ok(REAL, num)
          case _ => err(Scanner.Error.NUM_MULT_PERIOUS, num)
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
