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
// case object SingleQuote extends Token
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
// case class Quote(value: Expr) extends Expr
// case class Lambda(args: List[Identifier], body: Expr) extends Expr

object Main {
  type Prediate[T] = T => Boolean

  def main(args: Array[String]): Unit = {
    val sum = SExpr(List(Identifier("+"), Number(32), Number(43)))
    println(sum)
    // println(eval(sum))

    println(tokenize("(123 123 1 2 3)").toList)
    println(tokenize("""(display "hi")""").toList)
    println(tokenize("""(display "hi 1 2 3")""").toList)
    println(tokenize("""(display "hi 1 2 3" "1")""").toList)
    println(tokenize("""(#t #f #a #)""").toList)
    println(tokenize("(+ 1 2 3)").toList)
    println(parse(tokenize("(+ 1 2 3)")).toList)
  }

  // def eval(expr: Expr): Expr = {
  //   expr match {
  //     case SExpr(sexpr) =>
  //       sexpr match {
  //         case Identifier("+") :: Number(a) :: Number(b) :: Nil =>
  //           Number(a + b)
  //       }
  //   }
  // }

  def parse(ts: Iterator[Token]): Iterator[Expr] = {
    val tokens = ts.buffered
    for (t <- tokens)
      yield t match {
        case True => True
        case False => False
        case Number(n) => Number(n)
        case Str(str) => Str(str)
        case Identifier(id) => Identifier(id)
        case SExpr(vals) => SExpr(parse(vals.toIterator).toList)
        case OpenParen => SExpr(parse(consumeUntil[Token](tokens, { t => t == CloseParen })).toList)
        case CloseParen => InvalidExpr("unexpected closing paren")
      }
  }

  def tokenize(str: String): Iterator[Token] = {
    val src = str.toList.toIterator.buffered

    for (c <- src if !c.isWhitespace)
      yield c match {
        case '(' => OpenParen
        case ')' => CloseParen
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

  def consumeWhile[T](src: BufferedIterator[T], predicate: Prediate[T]): List[T] = {
    def aux(buff: List[T]): List[T] =
      if (src.hasNext && predicate(src.head)) {
        val curr = src.head
        src.next
        aux(buff ++ List(curr))
      } else buff

    aux(List.empty)
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
