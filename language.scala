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
  def main(args: Array[String]): Unit = {
    val sum = SExpr(List(Identifier("+"), Number(32), Number(43)))
    println(sum)
    println(eval(sum))
  }

  def eval(expr: Expr): Expr = {
    expr match {
      case SExpr(sexpr) =>
        sexpr match {
          case Identifier("+") :: Number(a) :: Number(b) :: Nil =>
            Number(a + b)
        }
    }
  }
}
