package xyz.minond.talk.pti

import Token._

class Scanner(raw: String) extends Iterator[Token] {
  val src = raw.trim.toList.toIterator.buffered

  def next(): Token = {
    src.next match {
      case ' '  => next
      case '('  => Token(OPEN_PAREN)
      case ')'  => Token(CLOSE_PAREN)
      case '#'  => Token(POUND)
      case '\'' => Token(SQUOTE)

      case '"' =>
        // XXX This should handle escaped quotes
        val str = consume(not(is('"')))
        src.next
        Token(STRING, Some(str.mkString))

      case n if isDigit(n) =>
        // XXX Should validate number here
        val nums = n :: consume(or(isDigit, is('.'))).toList
        Token(NUMBER, Some(nums.mkString))

      case x =>
        val chars = x :: consume(isIdentifier(_: Char)).toList
        Token(IDENTIFIER, Some(chars.mkString))
    }
  }

  // XXX Clean this function up
  def consume(f: Char => Boolean) = {
    var buff = List[Char]()

    while (src.hasNext && f(src.head)) {
      buff = buff ++ List(src.head)
      src.next
    }

    buff
  }

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
