package xyz.minond.talk.pti

object Token extends Enumeration {
  type Id = Value

  val OPEN_PAREN, CLOSE_PAREN, IDENTIFIER, STRING, INTEGER, REAL, POUND, QUOTE =
    Value
}

case class Token(id: Token.Id, lexeme: Option[String] = None) {
  import Token._

  override def toString: String = (id, lexeme) match {
    case (OPEN_PAREN | CLOSE_PAREN | QUOTE | POUND, _) =>
      s"($id)"
    case (STRING, Some(str)) =>
      s"""($id "$str")"""
    case (_, Some(value)) =>
      s"($id $value)"
    case (_, None) =>
      s"($id nil)"
  }
}

object Tokenizer {
  object Message {
    val STR_NO_CLOSING_WITH_CONT =
      """String did not end with a '"' but there is still input."""
    val STR_NO_CLOSING = """String did not end with a '"' character."""
    val NUM_MULT_PERIOUS = "Found multiple periods in number."
  }

  case class Error(message: String, lexeme: Option[String] = None)
}

class Tokenizer(raw: String) extends Iterator[Either[Tokenizer.Error, Token]] {
  import Token.{IDENTIFIER, POUND, INTEGER, REAL, OPEN_PAREN, CLOSE_PAREN, QUOTE, STRING}

  type CharComp = Char => Boolean

  val src = raw.trim.toList.toIterator.buffered

  def hasNext(): Boolean =
    src.hasNext

  def next(): Either[Tokenizer.Error, Token] = {
    src.next match {
      case c if c.isWhitespace => next

      case '(' => ok(OPEN_PAREN)
      case ')' => ok(CLOSE_PAREN)
      case '#' => ok(POUND)
      case '\'' => ok(QUOTE)

      case '"' =>
        val str = Some(lookbehind({
          case ('"', Some('\\')) => true
          case ('"', _) => false
          case _ => true
        }).mkString)

        (src.hasNext, if (src.hasNext) src.head else 0.toChar) match {
          case (true, '"') =>
            src.next
            ok(STRING, str)

          case (true, _) =>
            src.next
            err(Tokenizer.Message.STR_NO_CLOSING_WITH_CONT, str)

          case (false, _) =>
            err(Tokenizer.Message.STR_NO_CLOSING, str)
        }

      case n if n.isDigit =>
        val digits = n :: consume(or(_.isDigit, is('.'))).toList
        val num = Some(digits.mkString)

        digits.count(is('.')) match {
          case 0 => ok(INTEGER, num)
          case 1 => ok(REAL, num)
          case _ => err(Tokenizer.Message.NUM_MULT_PERIOUS, num)
        }

      case x =>
        val chars = x :: consume(isIdentifier(_: Char)).toList
        ok(IDENTIFIER, Some(chars.mkString))
    }
  }

  def ok(id: Token.Id, lexeme: Option[String] = None) =
    Right(Token(id, lexeme))

  def err(message: String, lexeme: Option[String] = None) =
    Left(Tokenizer.Error(message, lexeme))

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

  def consume(f: CharComp) =
    lookbehind((x: Char, _) => f(x))

  def isIdentifier(c: Char): Boolean =
    c != '(' && c != ')' && !c.isWhitespace

  def is(c: Char): CharComp =
    (x: Char) => c == x

  def not(f: CharComp): CharComp =
    (x: Char) => !f(x)

  def or(f1: CharComp, f2: CharComp): CharComp =
    (x: Char) => f1(x) || f2(x)
}
