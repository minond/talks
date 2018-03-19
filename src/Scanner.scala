package xyz.minond.talk.pti

class Scanner(raw: String) extends Iterator[Token] {
  import Token._

  val src = raw.toList.toIterator

  def hasNext(): Boolean = {
    src.hasNext
  }

  def next(): Token = {
    src.next match {
      case '(' => Token(OPEN_PAREN, None)
      case ')' => Token(CLOSE_PAREN, None)
      case 47  => Token(SQUOTE, None)
      case '#' => Token(POUND, None)
      case '"' => Token(IDENTIFIER, None)
      case _   => Token(IDENTIFIER, None)
    }
  }
}
