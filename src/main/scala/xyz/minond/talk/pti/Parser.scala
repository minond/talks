package xyz.minond.talk.pti

import Statement._

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

  def next(): Either[Error, Statement] =
    ???

  def ok(stmt: Statement) =
    Right(stmt)

  def err(message: String, stmt: Option[Statement] = None) =
    Left(Error(message, stmt))
}
