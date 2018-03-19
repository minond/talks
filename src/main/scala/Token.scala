package xyz.minond.talk.pti

object Token extends Enumeration {
  type Id = Value

  val OPEN_PAREN, CLOSE_PAREN, IDENTIFIER, STRING, INTEGER, REAL, POUND,
  SQUOTE =
    Value

  case class Error(message: String, lexeme: Option[String] = None)
}

case class Token(id: Token.Id, lexeme: Option[String] = None) {
  import Token._

  override def toString(): String = (id, lexeme) match {
    case (OPEN_PAREN | CLOSE_PAREN | SQUOTE | POUND, _) =>
      s"($id)"
    case (STRING, Some(str)) =>
      s"""($id "$str")"""
    case (_, Some(value)) =>
      s"($id $value)"
    case (_, None) =>
      s"($id nil)"
  }
}
