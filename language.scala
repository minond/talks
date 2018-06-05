import scala.collection.mutable.ListBuffer

/* Grammar:
 *
 * main    = { exprs } ;
 * number  = [ "-" ] , digit { digit } ;
 * digit   = 0 ... 9 ;
 * string  = '"' , { letter } , '"' ;
 * letter  = "A" ... "z" ;
 * boolean = "#t" | "#f" ;
 * identifier = identifier ,
 *         { letter | symbol | digit } ;
 * symbol  = "<" | ">" | "*" | "+" | "-"
 *         | "=" | "_" | "/" | "%" ;
 * atom    = identifier | number
 *         | boolean ;
 * exprs   = [ "'" ] ( atom | sexpr ) ;
 * sexpr   = "(" { exprs } ")" ;
 */

sealed trait Token
case class InvalidToken(lexeme: String) extends Token
case object SingleQuote extends Token
case object OpenParen extends Token
case object CloseParen extends Token

sealed trait Expr extends Token
case class InvalidExpr(message: String) extends Expr
case class Number(value: Double) extends Expr
case class Str(value: String) extends Expr
case object True extends Expr
case object False extends Expr
case class Identifier(value: String) extends Expr
case class SExpr(values: List[Expr]) extends Expr
case class Quote(value: Expr) extends Expr
case class Lambda(args: List[Identifier], body: Expr) extends Expr

object Main {
  type Prediate[T] = T => Boolean

  def main(args: Array[String]): Unit = {
    println(passLambdas(parse(tokenize("(d (+ 1 2))"))))
    println(passLambdas(parse(tokenize("(lambda (a b) (+ a b))"))))
    println(passLambdas(parse(tokenize("(lambda (a b c) +)"))))
    println(passLambdas(parse(tokenize("#t"))))
  }

  def passLambdas(expr: Expr): Expr = {
    expr match {
      case SExpr(Identifier("lambda") :: SExpr(args) :: body :: Nil) =>
        val (params, errs) = args.foldRight[(List[Identifier], List[InvalidExpr])](List.empty, List.empty){
          case (curr, (params, errs)) =>
            curr match {
              case id @ Identifier(_) => (id :: params, errs)
              case x => (params, InvalidExpr("S") :: errs)
            }
        }

        if (!errs.isEmpty) errs(0)
        else Lambda(params, body)

      case e => e
    }
  }

  def parse(ts: Iterator[Token]): Expr = {
    val tokens = ts.buffered
    val head = tokens.next
    head match {
      case InvalidToken(lexeme) => InvalidExpr(s"unexpected '$lexeme'")
      case InvalidExpr(msg) => InvalidExpr(msg)
      case True => True
      case False => False
      case Number(n) => Number(n)
      case Str(str) => Str(str)
      case Identifier(id) => Identifier(id)
      case SExpr(vals) => SExpr(vals)
      case Quote(expr) => Quote(expr)
      case Lambda(args, body) => Lambda(args, body)
      case SingleQuote =>
        if (tokens.hasNext) Quote(parse(List(tokens.next).toIterator))
        else InvalidExpr("missing expression after quote")
      case OpenParen =>
        def aux(tokens: BufferedIterator[Token]): List[Expr] =
          if (tokens.hasNext && tokens.head != CloseParen)
            parse(tokens) :: aux(tokens)
          else List.empty

        val values = aux(tokens)

        if (tokens.hasNext) {
          tokens.next
          SExpr(values.toList)
        } else InvalidExpr("missing ')'")

      case CloseParen => InvalidExpr("unexpected ')'")
    }
  }

  def tokenize(str: String): Iterator[Token] = {
    val src = str.toList.toIterator.buffered

    for (c <- src if !c.isWhitespace)
      yield c match {
        case '(' => OpenParen
        case ')' => CloseParen
        case '\'' => SingleQuote
        case '"' => Str(consumeUntil[Char](src, { c => c == '"' }).mkString)
        case n if isDigit(n) => Number((n + consumeWhile(src, isDigit).mkString).toDouble)
        case c if isIdentifier(c) => Identifier(c + consumeWhile(src, isIdentifier).mkString)

        case '#' => src.headOption match {
          case Some('f') => src.next; False
          case Some('t') => src.next; True
          case Some(c) => src.next; InvalidToken(s"#$c")
          case None => InvalidToken("#")
        }

        case c => InvalidToken(c + consumeWhile[Char](src, isWord).mkString)
      }
  }

  def consumeWhile[T](src: BufferedIterator[T], predicate: Prediate[T]): Iterator[T] = {
    def aux(buff: List[T]): List[T] =
      if (src.hasNext && predicate(src.head)) {
        val curr = src.head
        src.next
        aux(buff :+ curr)
      } else buff

    aux(List.empty).toIterator
  }

  def consumeUntil[T](src: BufferedIterator[T], predicate: Prediate[T]): Iterator[T] =
    src.takeWhile({ c => !predicate(c) })

  def isIdentifier(c: Char): Boolean =
    isDigit(c) || isLetter(c) || isSymbol(c)

  def isDigit(c: Char): Boolean =
    c >= '0' && c <= '9'

  def isLetter(c: Char): Boolean =
    c >= 'A' && c <= 'z'

  def isSymbol(c: Char): Boolean =
    Set('<', '>', '*', '+', '-', '=', '_', '/', '%').contains(c)

  def isParen(c: Char): Boolean =
    c == '(' || c == ')'

  def isWord(c: Char): Boolean =
    !c.isWhitespace && !isParen(c)
}
