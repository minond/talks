package xyz.minond.talk.pti

object Token extends Enumeration {
  type Id = Value

  val OPEN_PAREN, CLOSE_PAREN, IDENTIFIER, STRING, NUMBER, POUND, SQUOTE =
    Value
}

case class Token(id: Token.Id, lexeme: Option[String]) {
  import Token._

  override def toString(): String = (id, lexeme) match {
    case (OPEN_PAREN, _)     => "(OPEN_PAREN)"
    case (CLOSE_PAREN, _)    => "(CLOSE_PAREN)"
    case (SQUOTE, _)         => "(SQUOTE)"
    case (POUND, _)          => "(POUND)"
    case (STRING, Some(str)) => s"""($id "$str")"""
    case (_, Some(value))    => s"($id $value)"
    case (_, None)           => s"($id nil)"
  }
}
