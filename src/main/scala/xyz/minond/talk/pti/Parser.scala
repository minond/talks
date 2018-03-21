package xyz.minond.talk.pti

import Statement._
import Token.{IDENTIFIER, POUND}

object Parser {
  object Error {
    val STR_INVALID_BOOL = "Cannot parse boolean value."
    def STR_INVALID_BOOL_VALUE(value: String) =
      s"Expecting either 'f' or 't' but found '${value}' instead."
    def STR_INVALID_BOOL_TOK(token: Token) =
      s"Expecting either 'f' or 't' but found ${token} instead."

    val STR_INVALID_TOK = "Cannot parse invalid token."
    def STR_UNEXPECTED_TOK(token: Token) =
      s"Found unexpected token: ${token}"
    def STR_EXPECTING_ONE_OF(ids: Token.Id*) =
      s"Expecting (one of): ${ids.mkString(", ")}."
  }
}

/*
 * Grammar:
 *
 * MAIN     = { stmt } ;
 * stmt     = "'" stmt | sexpr | value ;
 * sexpr    = "(" { value } ")" ;
 * value    = IDENTIFIER | NUMBER | boolean ;
 * boolean  = "#" ( "f" | "t" ) ;
 */
class Parser(source: Tokenizer) extends Iterator[Either[Error, Statement]] {
  val tokens = source.buffered

  def hasNext(): Boolean =
    tokens.hasNext

  def next(): Either[Error, Statement] = {
    tokens.head match {
      case Right(Token(POUND, _)) => parseBoolean

      case Right(_) => ???

      case Left(Token.Error(msg, _)) =>
        Left(Error(Parser.Error.STR_INVALID_TOK, Some(Error(msg))))
    }
  }

  def expect(ids: Token.Id*): Either[Error, Token] = {
    tokens.head match {
      case Right(token) if ids contains token.id =>
        tokens.next
        Right(token)

      case Left(Token.Error(msg, _)) =>
        Left(Error(Parser.Error.STR_INVALID_TOK, Some(Error(msg))))

      case Right(token) =>
        Left(
          Error(Parser.Error.STR_UNEXPECTED_TOK(token),
                Some(Error(Parser.Error.STR_EXPECTING_ONE_OF(ids: _*)))))
    }
  }

  def parseBoolean() = {
    expect(POUND) match {
      case Left(err) =>
        Left(Error(Parser.Error.STR_INVALID_BOOL, Some(err)))

      case Right(_) =>
        expect(IDENTIFIER) match {
          case Right(Token(IDENTIFIER, Some("t"))) => Right(BooleanStmt(true))
          case Right(Token(IDENTIFIER, Some("f"))) => Right(BooleanStmt(false))

          case Right(Token(IDENTIFIER, Some(invalid))) =>
            tokens.next
            Left(
              Error(Parser.Error.STR_INVALID_BOOL,
                    Some(Error(Parser.Error.STR_INVALID_BOOL_VALUE(invalid)))))

          case Right(token) =>
            tokens.next
            Left(
              Error(Parser.Error.STR_INVALID_BOOL,
                    Some(Error(Parser.Error.STR_INVALID_BOOL_TOK(token)))))

          case Left(err) =>
            tokens.next
            Left(Error(Parser.Error.STR_INVALID_BOOL, Some(err)))
        }
    }
  }
}
