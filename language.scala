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
case class Invalid(lexeme: String) extends Token
case object SingleQuote extends Token
case object OpenParen extends Token
case object CloseParen extends Token

sealed trait Expr extends Token
case class Number(value: Double) extends Expr
case class Str(value: String) extends Expr
case object True extends Expr
case object False extends Expr
case class Identifier(value: String) extends Expr
case class SExpr(values: List[Expr]) extends Expr
case class Quote(value: Expr) extends Expr
case class Lambda(args: List[Identifier], body: Expr) extends Expr

object Main {
  type Prediate = (Char) => Boolean

  def main(args: Array[String]): Unit = {
    val sum = SExpr(List(Identifier("+"), Number(32), Number(43)))
    println(sum)
    // println(eval(sum))

    println(tokenize("(123 123 1 2 3)").toList)
    println(tokenize("""(display "hi")""").toList)
    println(tokenize("""(display "hi 1 2 3")""").toList)
    println(tokenize("""(display "hi 1 2 3" "1")""").toList)
    println(tokenize("""(#t #f #a #)""").toList)
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

  def tokenize(str: String): Iterator[Token] = {
    val src = str.toList.toIterator.buffered

    for (c <- src if !c.isWhitespace)
      yield c match {
        case '(' => OpenParen
        case ')' => CloseParen
        case '"' => Str(consumeUntil(src, { c => c == '"' }))
        case n if isDigit(n) => Number((n + consumeWhile(src, isDigit)).toDouble)
        case c if isIdentifier(c) => Identifier(c + consumeWhile(src, isIdentifier))

        case '#' => src.headOption match {
          case Some('f') => src.next; False
          case Some('t') => src.next; True
          case Some(c) => src.next; Invalid(s"#$c")
          case None => Invalid("#")
        }

        case c => Invalid(c + consumeWhile(src, { c => !c.isWhitespace && !isParen(c) }))
      }
  }

  def consumeWhile(src: BufferedIterator[Char], predicate: Prediate): String = {
    def aux(buff: List[Char]): List[Char] =
      if (src.hasNext && predicate(src.head)) {
        val curr = src.head
        src.next
        aux(buff ++ List(curr))
      } else buff

    aux(List.empty).mkString
  }

  def consumeUntil(src: BufferedIterator[Char], predicate: Prediate): String =
    src.takeWhile({ c => !predicate(c) }).toList.mkString

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
}
