package xyz.minond.talk.pti

import Token.{IDENTIFIER, POUND, INTEGER, REAL, OPEN_PAREN, CLOSE_PAREN, QUOTE, STRING}
import scala.collection.mutable.ListBuffer

trait Expression

case class BooleanExpr(value: Boolean) extends Expression
case class IdentifierExpr(value: String) extends Expression
case class IntNumberExpr(value: Int) extends Expression
case class QuoteExpr(value: Expression) extends Expression
case class RealNumberExpr(value: Double) extends Expression
case class SExpr(values: List[Expression]) extends Expression
case class StringExpr(value: String) extends Expression

case class Error(message: String, prev: Option[Error] = None)

object Parser {
  object Error {
    val STR_INVALID_INT = "Cannot parse integer number."
    val STR_INVALID_REAL = "Cannot parse real number."
    val STR_INVALID_SEXPR = "Cannot parse s-expression."

    val STR_INVALID_STR = "Cannot parse string."
    val STR_INVALID_NIL_STR = "Cannot parse empty string."

    val STR_INVALID_QUOTE = "Cannot parse quoted expression."
    val STR_INVALID_NIL_QUOTE = "Cannot parse empty quote expression."

    val STR_INVALID_IDENTIFIER = "Cannot parse identifier."
    val STR_INVALID_NIL_IDENTIFIER = "Empty identifier value."

    val STR_INVALID_BOOL = "Cannot parse boolean value."
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
class Parser(source: Tokenizer) extends Iterator[Either[Error, Expression]] {
  val tokens = source.buffered

  var curr =
    if (tokens.hasNext) tokens.head
    else Left(Token.Error("EOF"))

  def hasNext(): Boolean =
    tokens.hasNext

  def next(): Either[Error, Expression] = {
    tokens.head match {
      case Right(Token(POUND, _)) => parseBoolean
      case Right(Token(INTEGER, _)) => parseInteger
      case Right(Token(REAL, _)) => parseReal
      case Right(Token(IDENTIFIER, _)) => parseIdentifier
      case Right(Token(QUOTE, _)) => parseQuote
      case Right(Token(OPEN_PAREN, _)) => parseSExpr
      case Right(Token(STRING, _)) => parseString

      case Right(Token(CLOSE_PAREN, _)) =>
        skip
        Left(Error(Parser.Error.STR_UNEXPECTED_TOK(Token(CLOSE_PAREN))))

      case Right(token) =>
        skip
        Left(Error(Parser.Error.STR_UNEXPECTED_TOK(token)))

      case Left(Token.Error(msg, _)) =>
        skip
        Left(Error(Parser.Error.STR_INVALID_TOK, Some(Error(msg))))
    }
  }

  def eat() = {
    curr = tokens.head
    skip
    curr
  }

  def skip() =
    if (tokens.hasNext) tokens.next

  def expect(ids: Token.Id*): Either[Error, Token] = {
    eat match {
      case Right(token) if ids contains token.id =>
        Right(token)

      case Left(Token.Error(msg, _)) =>
        Left(Error(Parser.Error.STR_INVALID_TOK, Some(Error(msg))))

      case Right(token) =>
        Left(
          Error(
            Parser.Error.STR_UNEXPECTED_TOK(token),
            Some(Error(Parser.Error.STR_EXPECTING_ONE_OF(ids: _*)))))
    }
  }

  def parseBoolean() = {
    (expect(POUND), expect(IDENTIFIER)) match {
      case (Left(err), _) => Left(Error(Parser.Error.STR_INVALID_BOOL, Some(err)))
      case (_, Left(err)) => Left(Error(Parser.Error.STR_INVALID_BOOL, Some(err)))

      case (Right(_), Right(Token(_, Some("t")))) => Right(BooleanExpr(true))
      case (Right(_), Right(Token(_, Some("f")))) => Right(BooleanExpr(false))

      case (Right(_), Right(token)) =>
        Left(
          Error(
            Parser.Error.STR_INVALID_BOOL,
            Some(Error(Parser.Error.STR_INVALID_BOOL_TOK(token)))))
    }
  }

  def parseInteger() = {
    (expect(INTEGER), curr.map { _.lexeme.getOrElse("").toInt }) match {
      case (Left(err), _) =>
        Left(Error(Parser.Error.STR_INVALID_INT, Some(err)))

      case (_, Left(err)) =>
        Left(Error(Parser.Error.STR_INVALID_INT, Some(Error(err.message))))

      case (Right(_), Right(value)) =>
        Right(IntNumberExpr(value))
    }
  }

  def parseReal() = {
    (expect(REAL), curr.map { _.lexeme.getOrElse("").toDouble }) match {
      case (Left(err), _) =>
        Left(Error(Parser.Error.STR_INVALID_REAL, Some(err)))

      case (_, Left(err)) =>
        Left(Error(Parser.Error.STR_INVALID_REAL, Some(Error(err.message))))

      case (Right(_), Right(value)) =>
        Right(RealNumberExpr(value))
    }
  }

  def parseIdentifier() = {
    expect(IDENTIFIER) match {
      case Left(err) =>
        Left(Error(Parser.Error.STR_INVALID_IDENTIFIER, Some(err)))

      case Right(Token(_, None)) =>
        Left(
          Error(
            Parser.Error.STR_INVALID_IDENTIFIER,
            Some(Error(Parser.Error.STR_INVALID_NIL_IDENTIFIER))))

      case Right(Token(_, Some(value))) =>
        Right(IdentifierExpr(value))
    }
  }

  def parseSExpr(): Either[Error, Expression] = {
    expect(OPEN_PAREN) match {
      case Left(err) =>
        Left(Error(Parser.Error.STR_INVALID_SEXPR, Some(err)))

      case Right(_) =>
        val values = ListBuffer[Expression]()

        while (tokens.hasNext && tokens.head != Right(Token(CLOSE_PAREN))) {
          next match {
            case Left(err) =>
              return Left(Error(Parser.Error.STR_INVALID_SEXPR, Some(err)))

            case Right(stmt) =>
              values += stmt
          }
        }

        expect(CLOSE_PAREN) match {
          case Left(err) => Left(Error(Parser.Error.STR_INVALID_SEXPR, Some(err)))
          case Right(_) => Right(SExpr(values.toList))
        }
    }
  }

  def parseQuote() = {
    (expect(QUOTE), hasNext) match {
      case (Left(err), _) => Left(Error(Parser.Error.STR_INVALID_QUOTE, Some(err)))
      case (_, false) => Left(Error(Parser.Error.STR_INVALID_NIL_QUOTE))

      case (Right(_), true) =>
        next match {
          case Left(err) => Left(Error(Parser.Error.STR_INVALID_QUOTE, Some(err)))
          case Right(stmt) => Right(QuoteExpr(stmt))
        }
    }
  }

  def parseString() = {
    expect(STRING) match {
      case Left(err) => Left(Error(Parser.Error.STR_INVALID_STR, Some(err)))
      case Right(Token(_, None)) => Left(Error(Parser.Error.STR_INVALID_NIL_STR))
      case Right(Token(_, Some(str))) => Right(StringExpr(str))
    }
  }
}
