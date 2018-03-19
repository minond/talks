package xyz.minond.talk.pti

class Scanner(raw: String) extends Iterator[Token] {
  import Token._

  val src = raw.trim.toList.toIterator.buffered

  def next(): Token = {
    src.next match {
      case ' '  => next
      case '('  => Token(OPEN_PAREN, None)
      case ')'  => Token(CLOSE_PAREN, None)
      case '#'  => Token(POUND, None)
      case '\'' => Token(SQUOTE, None)

      case '"' =>
        // XXX This should handle escaped quotes
        val str = src.takeWhile(not(is('"')))
        Token(STRING, Some(str.mkString))

      case n if isDigit(n) =>
        // XXX Should validate number here
        val nums = n :: src.takeWhile(or(isDigit, is('.'))).toList
        Token(NUMBER, Some(nums.mkString))

      case h =>
        val chars = h :: src.takeWhile(isIdentifier).toList
        Token(IDENTIFIER, Some(chars.mkString))
    }
  }

  def hasNext(): Boolean =
    src.hasNext

  def isDigit(c: Char): Boolean =
    c >= '0' && c <= '9'

  def isIdentifier(c: Char): Boolean =
    c != '(' && c != ')' && c != ' '

  def is(c: Char) =
    (x: Char) => c == x

  def not(f: (Char) => Boolean) =
    (x: Char) => !f(x)

  def or(f1: (Char) => Boolean, f2: (Char) => Boolean) =
    (x: Char) => f1(x) || f2(x)
}
