package xyz.minond.talk.pti

object Main {
  def main(args: Array[String]): Unit = {
    import Token._

    val tokens = Array(
      Token(OPEN_PAREN, None),
      Token(IDENTIFIER, Some("+")),
      Token(NUMBER, Some("2")),
      Token(CLOSE_PAREN, None)
    )

    tokens.map { println }
  }
}
