import scala.collection.mutable.ListBuffer

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
    println(parse(tokenize("(d (+ 1 2))")))
    println(parse(tokenize("(lambda (a b) (+ a b))")))
    println(parse(tokenize("((lambda (a 1) (+ a b)) 40 2)")))
    println(parse(tokenize("(lambda (a b c) +)")))
    println(parse(tokenize("#t")))
    println(parse(tokenize("123")))
    println(parse(tokenize("1 2 3")))
    println(parse(tokenize(""""a b c"""")))
    println(parse(tokenize("''''(1 2 3)")))
  }

  def passErrors(expr: Expr): Expr =
    expr match {
      case SExpr(xs) =>
        xs flatMap {
          case err @ InvalidExpr(_) => Some(err)
          case _ => None
        } match {
          case Nil => SExpr(xs)
          case err :: _ => err
        }

      case err @ InvalidExpr(_) => err
      case Quote(err @ InvalidExpr(_)) => err
      case Lambda(_, err @ InvalidExpr(_)) => err
      case expr => expr
    }

  def passLambdas(expr: Expr): Expr =
    expr match {
      case SExpr(Identifier("lambda") :: SExpr(args) :: body :: Nil) =>
        val (params, errs) = args.foldRight[(List[Identifier], List[InvalidExpr])](List.empty, List.empty){
          case (curr, (params, errs)) =>
            curr match {
              case id @ Identifier(_) => (id :: params, errs)
              case x =>
                val msg = s"expecting identifier in lambda argument but got $x"
                (params, InvalidExpr(msg) :: errs)
            }
        }

        if (!errs.isEmpty) errs(0)
        else Lambda(params, body)

      case expr => expr
    }

  def parse(ts: Iterator[Token]): Expr = {
    val tokens = ts.buffered
    passErrors(passLambdas(tokens.next match {
      case expr @ (True | False | _: Str | _: Number | _: Identifier
          | _: SExpr | _: Quote | _: Lambda | _: InvalidExpr) =>
        expr.asInstanceOf[Expr]

      case SingleQuote =>
        if (tokens.hasNext) Quote(parse(tokens))
        else InvalidExpr("missing expression after quote")

      case OpenParen =>
        def aux(tokens: BufferedIterator[Token]): List[Expr] =
          if (tokens.hasNext && tokens.head != CloseParen)
            parse(tokens) :: aux(tokens)
          else List.empty

        val values = aux(tokens)

        if (tokens.hasNext) {
          tokens.next
          SExpr(values)
        } else InvalidExpr("missing ')'")

      case InvalidToken(lexeme) =>
        InvalidExpr(s"unexpected '$lexeme'")

      case CloseParen =>
        InvalidExpr("unexpected ')'")
    }))
  }

  def tokenize(str: String): Iterator[Token] = {
    val src = str.toList.toIterator.buffered

    for (c <- src if !c.isWhitespace)
      yield c match {
        case '(' => OpenParen
        case ')' => CloseParen
        case '\'' => SingleQuote
        case '"' => Str(consumeUntil(src, is('"')).mkString)
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

  def is[T](x: T): Prediate[T] =
    (y: T) => x == y
}
