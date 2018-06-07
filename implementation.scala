import scala.collection.mutable.ListBuffer

object Main {
  type Prediate[T] = T => Boolean
  type Env = Map[Identifier, Expr]

  sealed trait Token
  case class InvalidToken(lexeme: String) extends Token
  case object SingleQuote extends Token
  case object OpenParen extends Token
  case object CloseParen extends Token

  sealed trait Expr extends Token
  case class Err(message: String) extends Expr
  case class Number(value: Double) extends Expr
  case class Str(value: String) extends Expr
  case object True extends Expr
  case object False extends Expr
  case class Identifier(value: String) extends Expr
  case class SExpr(values: List[Expr]) extends Expr
  case class Quote(value: Expr) extends Expr
  case class Lambda(args: List[Identifier], body: Expr) extends Expr
  case class Proc(f: (List[Expr], Env) => (Expr, Env)) extends Expr
  case class Builtin(f: (List[Expr], Env) => (Expr, Env)) extends Expr

  val env = Map(
    Identifier("define") -> Builtin((args, env) =>
      args match {
        case (id @ Identifier(_)) :: expr :: Nil =>
          (Quote(Str("ok")), env ++ Map(id -> expr))
        case _ =>
          (Err("bad call to define. expecting an identifier and a value"), env)
    }),
    Identifier("begin") -> Builtin((args, env) => {
      val (last, _) =
        args.foldLeft[(Expr, Env)]((Quote(Str("nil")), env)) {
          case ((_, env), expr) => evaluate(expr, env)
        }

      (last, env)
    }),
    Identifier("cond") -> Builtin((args, env) => {
      def aux(conds: List[Expr]): Expr =
        conds match {
          case SExpr(check :: body :: Nil) :: rest =>
            evaluate(check, env)._1 match {
              case False => aux(rest)
              case _ => evaluate(body, env)._1
            }
          case Nil => Quote(Str("nil"))
          case _ => Err("bad syntax. cond expects a list of expression pairs")
        }

      (aux(args), env)
    }),
    Identifier("add") -> Proc((args, env) =>
      (args match {
        case Number(a) :: Number(b) :: Nil => Number(a + b)
        case _ => Err("bad call to add. expecting two numbers")
      }, env))
  )

  def main(args: Array[String]): Unit = {
    run("((lambda (x) (add x 20)) 22)")
    run("((lambda (a b) (add a b)) 5 -10)")
    run("((lambda (a b) (add a b)) -10 5)")
    run("(begin (define -a -123) (add -a -a))")
    run("(add -123 123)")
    run("(d (+ 1 2))")
    run("(lambda (a b) (+ a b))")
    run("((lambda (a 1) (+ a b)) 40 2)")
    run("(lambda (a b c) +)")
    run("#t")
    run("123")
    run("1 2 3")
    run(""""a b c"""")
    run("''''(1 2 3)")
    run("((lambda (a b) (add a b)) 40 2)")
    run("(123 1 2)")
    run("(define two 2)")
    run("(cond (#f 123) (#t 321))")
    run("""
    (begin
      (define a 123)
      (define b 321)
      (add a b))
    """)
  }

  def run(src: String, env: Env = env): Expr = {
    val expr = evaluate(parse(tokenize(src)), env)._1
    println(s"<= ${src.trim}\n=> $expr")
    expr
  }

  def evaluate(expr: Expr, env: Env): (Expr, Env) =
    expr match {
      case expr @ (True | False | _: Str | _: Number | _: Quote | _: Lambda | _: Builtin |
          _: Proc | _: Err) =>
        (expr, env)

      case id @ Identifier(name) =>
        val value = env.getOrElse(id, Err(s"unbound variable: $id"))
        (value, env)

      case SExpr((id @ Identifier(_)) :: body) =>
        val (head, _) = evaluate(id, env)
        evaluate(SExpr(head :: body), env)

      case SExpr(Lambda(args, body) :: values) =>
        evaluate(body, args.zip(values).foldLeft(env) {
          case (env, (arg, value)) => env ++ Map(arg -> value)
        })

      case SExpr(Proc(fn) :: args) =>
        fn(args.map { arg =>
          evaluate(arg, env)._1
        }, env)

      case SExpr(Builtin(fn) :: args) =>
        fn(args, env)

      case SExpr((err @ Err(_)) :: _) =>
        (err, env)

      case SExpr(head :: _) =>
        (Err(s"bad s-expression, head cannot be $head"), env)

      case SExpr(Nil) =>
        (Err("empty s-expression not allowed"), env)
    }

  def passErrors(expr: Expr): Expr =
    expr match {
      case SExpr(xs) =>
        xs flatMap {
          case err @ Err(_) => Some(err)
          case _ => None
        } match {
          case Nil => SExpr(xs)
          case err :: _ => err
        }

      case err @ Err(_) => err
      case Quote(err @ Err(_)) => err
      case Lambda(_, err @ Err(_)) => err
      case expr => expr
    }

  def passLambdas(expr: Expr): Expr =
    expr match {
      case SExpr(Identifier("lambda") :: SExpr(args) :: body :: Nil) =>
        val (params, errs) =
          args.foldRight(List[Identifier](), List[Err]()) {
            case (curr, (params, errs)) =>
              curr match {
                case id @ Identifier(_) => (id :: params, errs)
                case x =>
                  val msg = s"expecting identifier in lambda argument but got $x"
                  (params, Err(msg) :: errs)
              }
          }

        if (!errs.isEmpty) errs(0)
        else Lambda(params, body)

      case expr => expr
    }

  def parse(ts: Iterator[Token]): Expr = {
    val tokens = ts.buffered
    passErrors(passLambdas(tokens.next match {
      case expr @ (True | False | _: Str | _: Number | _: Identifier | _: SExpr |
          _: Quote | _: Lambda | _: Builtin | _: Proc | _: Err) =>
        expr.asInstanceOf[Expr]

      case SingleQuote =>
        if (tokens.hasNext) Quote(parse(tokens))
        else Err("missing expression after quote")

      case OpenParen =>
        def aux(tokens: BufferedIterator[Token]): List[Expr] =
          if (tokens.hasNext && tokens.head != CloseParen)
            parse(tokens) :: aux(tokens)
          else List.empty

        val values = aux(tokens)

        if (tokens.hasNext) {
          tokens.next
          SExpr(values)
        } else Err("missing ')'")

      case InvalidToken(lexeme) =>
        Err(s"unexpected '$lexeme'")

      case CloseParen =>
        Err("unexpected ')'")
    }))
  }

  def tokenize(str: String): Iterator[Token] = {
    val src = str.toList.toIterator.buffered

    for (c <- src if !c.isWhitespace)
      yield
        c match {
          case '(' => OpenParen
          case ')' => CloseParen
          case '\'' => SingleQuote
          case '"' => Str(src.takeWhile(not(is('"'))).mkString)
          case n if isDigit(n) || (is('-')(n) && isDigit(src.head)) =>
            Number((n + consumeWhile(src, isDigit).mkString).toDouble)
          case c if isIdentifierStart(c) =>
            Identifier(c + consumeWhile(src, isIdentifier).mkString)

          case '#' =>
            src.headOption match {
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

  def isIdentifierStart(c: Char): Boolean =
    isLetter(c) || isSymbol(c)

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

  def not[T](f: Prediate[T]): Prediate[T] =
    (x: T) => !f(x)
}
