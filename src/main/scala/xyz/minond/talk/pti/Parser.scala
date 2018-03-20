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
class Parser(tokens: Scanner) extends Iterator[Either[Error, Statement]] {
  def next: Either[Error, Statement] = ???

  def hasNext(): Boolean =
    tokens.hasNext
}
