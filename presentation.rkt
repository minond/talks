#lang slideshow

(require
  slideshow/code
  slideshow/text)

(current-main-font "Baskerville")
(current-code-font "IBM Plex Mono")
(code-colorize-enabled #f)

(define (unmargin line)
  (let ([parts (string-split line "|")])
    (string-trim (if (= 1 (length parts))
                   (car parts)
                   (car (cdr parts))))))

(define (mono texts #:ratio [ratio 1] #:fill [fill #f])
  (apply vl-append 1
         (for/list ([line (string-split (string-trim texts) "\n")])
           (with-size ((get-current-code-font-size))
                      (with-font (current-code-font)
                                 (para #:fill? fill
                                       #:width (* ratio (current-para-width))
                                       #:align 'left
                                       line))))))

(define (code str)
  (with-size (- (current-font-size) 6)
             (with-font (current-code-font)
                        (t str))))

(define (ordered . items)
  (apply vl-append gap-size
         (for/list ([x items]
                    [n (range 1 (+ 1 (length items)))])
           (item
             #:gap-size 0
             #:bullet (t (format "~s - " n))
             x))))

(define (unordered . items)
  (apply vl-append gap-size
         (for/list ([x items])
           (item x))))

(define (p txt)
  (para (string-normalize-spaces txt)))

(slide
  #:layout 'center
  (titlet "From parsing to interpretation")
  (t "Let’s build a language"))

(slide
  #:title "What are we talking about?"
  (p "We're going to be talking about programming languages."))

(slide
  #:title "What are we talking about?"
  (p "More specifically, we're going to be talking about interpreters."))

(slide
  #:title "What are we talking about?"
  (p "And even more specifically than that, we're going to talk about how one
     can take a sequence of characters that only a human could understand and
     make a computer understand them."))

(slide
  #:title "And why would we talk about that?"
  (p "Well, we're Software Engineers and as Software Engineers we write a lot
     of code."))

(slide
  #:title "And why would we talk about that?"
  (p "And how do we write that code? Well, with programming languages."))

(slide
  #:title "And why would we talk about that?"
  (p "Programming languages are tools. Can you think of a tool that you use
     more often?")
  (p "Most likely no."))

(slide
  #:title "And why would we talk about that?"
  (p "An understanding of programming languages and their implementation, even
     at a high level, will help you improve as a developer. Even if these
     skills are not used every day, the knowledge will stay with you and help
     you throughout your career."))

(slide
  #:layout 'center
  (t "So what are we going to do about it?"))

(slide
  #:layout 'center
  (t "Let’s build an interpreter"))

(slide
  #:title "What's that?"
  (scale (bitmap (build-path "assets" "interpreter-definitions.png")) .5))

(slide
  #:title "A program that can analyze a program"
  (scale (bitmap (build-path "assets" "interpreter-input.png")) .5))

(slide
  #:title "A program that can analyze a program"
  (scale (bitmap (build-path "assets" "interpreter-output.png")) .5))

(slide
  #:layout 'center
  (t "Where do we start?"))

(slide
  #:layout 'center
  (t "How about with fancy buzzwords?"))

(slide
  #:title "Ohh, fancy."
  (unordered
    "Grammars"
    "BNF/EBNF"
    "Lexer"
    "Parsers"
    "Parser generators"
    "Recursive descent parsers"
    "Scope"
    "Evaluation"))

(slide
  #:title "Where do we really start?"
  (ordered
    "Parse"
    "Evaluate"))

(slide
  #:title "This is where we start"
  (ordered
    "Define what our language looks like."
    "Tokenize the input into a stream of valid tokens."
    "Take the stream of tokens and compose them into complete expressions."
    "Evaluate the expressions."))

(slide
  #:layout 'center
  (t "Let’s define a language"))

(slide
  #:layout 'center
  (t "What can our language do?"))

(slide
  #:title "It can understand numbers"
  #:layout 'center
  (mono "7"))

(slide
  #:title "It can understand strings"
  #:layout 'center
  (mono "\"Hello, world.\""))

(slide
  #:title "It can understand something is true"
  #:layout 'center
  (mono "#t"))

(slide
  #:title "It can understand something is false"
  #:layout 'center
  (mono "#f"))

(slide
  #:title "It can run code conditionally"
  (mono #<<CODE
(cond (condition1 expression1)
      (condition2 expression2)
      (condition3 expression3)
      (condition4 expression4)
      (else default-expression))
CODE
))

(slide
  #:title "It can express arithmetic operations"
  #:layout 'center
  (mono "(* 21 2)"))

(slide
  #:title "It can define functions"
  (mono "(lambda (n) (* n 2))"))

(slide
  #:title "It can apply functions to parameters"
  #:layout 'center
  (mono "(double 21)"))

(slide
  #:title "It can store all of those values"
  (mono "(define cool #t)")
  (mono "(define age 99)")
  (mono "(define name \"Marcos\")")
  (mono "(define double (lambda (n) (* n 2)))"))

(slide
  #:title "Does it look familiar?"
  #:layout 'center
  (p "Yes, it looks like a Lisp. Notice all of those parenthesized lists? Those
      are s-expressions and we’ll be talking about them again soon."))

(slide
  #:layout 'center
  (t "Let’s get a little more specific"))

(slide
  #:layout 'center
  (t "Let’s build a BNF grammar"))

(slide
  #:title "What's BNF?"
  (p "Think of BNF as a language for languages. It's used in defining the
     structure in a computer language (and just not a programming language)"))

(slide
  #:title "What's BNF?"
  (para "BNF is made up of rules and their expansions, such as:"
        (code "<expr> ::= <digit> \"+\" <digit>")
        "where"
        (code "<expr>")
        "and"
        (code "<digit>")
        "are non-terminal symbols.")
  (para "And terminal symbols:"
        (code "<digit> ::= \"1\" | \"2\" | \"3\"")))

(slide
  #:layout 'center
  (t "Let’s build an EBNF grammar"))

(slide
  #:title "What's EBNF?"
  (para (string-normalize-spaces "EBNF is a set of extensions and modifications
          placed on top of BNF. Differences include dropping of the angled
          brackets,")
        (code "::=")
        "becomes"
        (code "=")
        ", and we add semicolons at the end of expressions.")
  (para "Other improvements include the ability to repeat expressions with"
        (code "{}")
        ", group expressions with"
        (code "()")
        ", add optional expressions with"
        (code "[]")
        ", and explicit concatenation with"
        (code ",")
        "."))

(slide
  #:layout 'center
  (t "Some examples?"))

(slide
  #:title "Numbers"
  (mono #<<CODE
number = [ "-" ] , digit { digit } ;

digit = "0" | "1" | "2" | "3" | "4"
      | "5" | "6" | "7" | "8" | "9" ;
CODE
))

(slide
  #:title "Strings"
  (mono #<<CODE
string = '"' { letter } '"' ;

letter = "A" | "B" | "C" | "D" | "E"
       | "F" | "G" | "H" | "I" | "J"
       | "K" | "L" | "M" | "N" | "O"
       | "P" | "Q" | "R" | "S" | "T"
       | "U" | "V" | "W" | "X" | "Y"
       | "Z" | "a" | "b" | "c" | "d"
       | "e" | "f" | "g" | "h" | "i"
       | "j" | "k" | "l" | "m" | "n"
       | "o" | "p" | "q" | "r" | "s"
       | "t" | "u" | "v" | "w" | "x"
       | "y" | "z" ;
CODE
))

(slide
  #:title "Booleans"
  (mono #<<CODE
boolean = "#t" | "#f" ;
CODE
))

(slide
  #:title "Identifiers"
  (mono #<<CODE
symbol = "<" | ">" | "*" | "+" | "-"
       | "=" | "_" | "/" | "%" ;

identifier = ( letter | symbol ) ,
   { letter | symbol | digit } ;
CODE
))

(slide
  #:title "S-expressions"
  (mono #<<CODE
sexpr = "(" { exprs } ")" ;

exprs = [ "'" ]
      ( atom | sexpr | exprs ) ;

atom = identifier | number
     | boolean | string ;
CODE
))

(slide
  #:title "All together now. I present to you our Lisp."
  (mono #<<CODE
main    = { exprs } ;
number  = [ "-" ] , digit { digit } ;
digit   = "0" | ... | "9" ;
string  = '"' , { letter } , '"' ;
letter  = "A" | ... | "z" ;
boolean = "#t" | "#f" ;
identifier = ( letter | symbol ) ,
        { letter | symbol | digit } ;
symbol  = "<" | ">" | "*" | "+" | "-"
        | "=" | "_" | "/" | "%" ;
atom    = identifier | number
        | boolean | string ;
exprs   = [ "'" ]
        ( atom | sexpr | exprs ) ;
sexpr   = "(" { exprs } ")" ;
CODE
))

(slide
  #:title "What does this give us?"
  (p "A reference for our ourselves or for a tool. A parser generator (like
     Yacc, GNU bison, ANTLR, etc.) could take our EBNF grammar and generate all
     of the code we need in order to parse our language.")
  (p "But that's not what we're here for."))

(slide
  #:layout 'center
  (t "Let’s build a parser"))

(slide
  #:title "But wait!"
  (p "Actually, let’s take a step back. Characters are hard but what if we had
     ‘words’ instead? We need a lexer."))

(slide
  #:title "What's a lexer?"
  (p "Lexers analyze a string, character by character, and turn it into a
     series of tokens that can be used in the later steps of parsing.")
  (blank)
  (hc-append (* 3 gap-size)
    (mono "(+ 21 43)")
    (arrow gap-size 0)
    (mono #<<CODE
OPAREN
ID(+)
NUM(21)
NUM(43)
CPAREN
CODE
)))

(slide
  #:title "Token types"
  (mono #:ratio 1.3 #<<CODE
sealed trait Token

case object SingleQuote extends Token
case object OpenParen extends Token
case object CloseParen extends Token
case object True extends Token
case object False extends Token

case class Number(value: Double)
  extends Token
case class Str(value: String)
  extends Token
CODE
))

(slide
  #:title "And even more tokens"
  (mono #:ratio 1.3 #<<CODE
case class InvalidToken(lexeme: String)
  extends Token

case class Identifier(value: String)
  extends Token

case class SExpr(values: List[Token])
  extends Token
CODE
))

(slide
  #:title "Tokenizer function"
  #:layout 'top
  (mono #:ratio 1.3 #<<CODE
def tokenize(str: String): Iterator[Token] = {
  val src = str.toList.toIterator.buffered
  for (c <- src if !c.isWhitespace)
    yield c match {
      // ...
    }
}
CODE
))

(slide
  #:title "Tokenizer function"
  #:layout 'top
  (mono #:ratio 1.3 #<<CODE
def tokenize(str: String): Iterator[Token] = {
  val src = str.toList.toIterator.buffered
  for (c <- src if !c.isWhitespace)
    yield c match {
      case '(' => OpenParen
      case ')' => CloseParen
      case '\'' => SingleQuote
      // ...
    }
}
CODE
))

(slide
  #:title "Tokenizer function"
  #:layout 'top
  (mono #:ratio 1.3 #<<CODE
def tokenize(str: String): Iterator[Token] = {
  val src = str.toList.toIterator.buffered
  for (c <- src if !c.isWhitespace)
    yield c match {
      case '(' => OpenParen
      case ')' => CloseParen
      case '\'' => SingleQuote
      case '"' => ???
      case n if isDigit(n) => ???
      case c if isIdentifier(c) => ???
      case '#' => ???
      case c => ???
    }
}
CODE
))

(slide
  #:title "Tokenizing strings"
  (mono #:ratio 1.3 #<<CODE
val src = str.toList.toIterator.buffered

yield c match {
  case '"' =>
    Str(src.takeWhile(c => c != '"')
      .mkString)
}
CODE
))

(slide
  #:title "Tokenizing numbers"
  (mono #:ratio 1.3 #<<CODE
val src = str.toList.toIterator.buffered

yield c match {
  case n if isDigit(n) ||
      (n == '-' && isDigit(src.head)) =>

    val num =
      (n + consumeWhile(src, isDigit).mkString)

    Number(num.toDouble)
}
CODE
))

(slide
  #:title "Helper definitions"
  (mono #:ratio 1.3 #<<CODE
def isDigit(c: Char): Boolean =
  c >= '0' && c <= '9'

def consumeWhile[T](
  src: BufferedIterator[T],
  predicate: T => Boolean
): Iterator[T] = {
  def aux(buff: List[T]): List[T] =
    if (src.hasNext && predicate(src.head)) {
      val curr = src.head
      src.next ; aux(buff :+ curr)
    } else buff

  aux(List.empty).toIterator
}
CODE
))

(slide
  #:title "Tokenizing identifiers"
  (mono #:ratio 1.3 #<<CODE
val src = str.toList.toIterator.buffered

yield c match {
  case c if isIdentifierStart(c) =>
    val name =
      c + consumeWhile(src, isIdentifier)

    Identifier(name.mkString)
}
CODE
))

(slide
  #:title "Helper definitions"
  (mono #:ratio 1.3 #<<CODE
def isIdentifierStart(c: Char): Boolean =
  isLetter(c) || isSymbol(c)

def isIdentifier(c: Char): Boolean =
  isDigit(c) || isLetter(c) || isSymbol(c)

def isLetter(c: Char): Boolean =
  c >= 'A' && c <= 'z'

def isSymbol(c: Char): Boolean =
  Set(
    '<', '>', '*', '+', '-',
    '=', '_', '/', '%'
  ).contains(c)
CODE
))

(slide
  #:title "Tokenizing booleans"
  (mono #:ratio 1.3 #<<CODE
val src = str.toList.toIterator.buffered

yield c match {
  case '#' =>
    src.headOption match {
      case None =>
        InvalidToken("unexpected <eof>")
      case Some('f') => src.next; False
      case Some('t') => src.next; True
      case Some(c) =>
        src.next; InvalidToken(s"#$c")
    }
}
CODE
))

(slide
  #:title "And now we have tokens"
  (vc-append (* 2 gap-size)
    (mono #<<CODE
tokenize("(+ 21 43)").toList
CODE
)
    (arrow gap-size (* pi 1.5))
    (mono #<<CODE
List(
  OpenParen,
  Identifier(+),
  Number(21.0),
  Number(43.0),
  CloseParen
)
CODE
)))

(slide
  #:title "Getting there"
  (p "We nearly have a full representation of our grammar. So far we've covered
     cases the following cases: numbers, strings, booleans, and identifier. But
     we're still missing the structured expressions: s-expressions."))

(slide
  #:title "We need these"
  (mono #<<CODE
sexpr   = "(" { exprs } ")" ;

exprs   = [ "'" ]
        ( atom | sexpr | exprs ) ;

atom    = identifier | number
        | boolean | string ;
CODE
))

(slide
  #:title "We need this"
  (hc-append (* 2 gap-size)
    (mono "(+ 21 43)")
    (arrow gap-size 0)
    (mono #<<CODE
OPAREN
ID(+)
NUM(21)
NUM(43)
CPAREN
CODE
)
    (arrow gap-size 0)
    (mono #<<CODE
SEXPR(
  ID(+),
  NUM(21),
  NUM(43))
CODE
)))

(slide
  #:title "ASTs"
  (p "An abstract syntax tree is a tree representation of source code
     structure. ASTs represent some tokens explicitly, like numbers, booleans,
     etc. and other implicitly, like parentheses and semicolons."))

(slide
  #:layout 'center
  (t "Let’s extend our data structures to match that"))

(slide
  #:title "Implicit data"
  (mono #:ratio 1.3 #<<CODE
sealed trait Token
case object SingleQuote extends Token
case object OpenParen extends Token
case object CloseParen extends Token
CODE
))

(slide
  #:title "Explicit data"
  (mono #:ratio 1.3 #<<CODE
sealed trait Expr extends Token
case object True extends Expr
case object False extends Expr
case class Number(value: Double) extends Expr
case class Str(value: String) extends Expr
case class Identifier(value: String)
  extends Expr
case class SExpr(values: List[Expr])
  extends Expr
CODE
))

(slide
  #:title "More expressions"
  (mono #:ratio 1.3 #<<CODE
case class Err(message: String) extends Expr
case class Quote(value: Expr) extends Expr
case class Lambda(args: List[Identifier],
  body: Expr) extends Expr

case class Proc(f: (List[Expr], Env)
  => (Expr, Env)) extends Expr

case class Builtin(f: (List[Expr], Env)
  => (Expr, Env)) extends Expr
CODE
))

(slide
  #:title "Parser function"
  #:layout 'top
  (mono #:ratio 1.3 #<<CODE
def parse(ts: Iterator[Token]): Expr = {
  val tokens = ts.buffered
  tokens.next match {
    // ...
  }
}
CODE
))

(slide
  #:title "Parser function"
  #:layout 'top
  (mono #:ratio 1.3 #<<CODE
def parse(ts: Iterator[Token]): Expr = {
  val tokens = ts.buffered
  tokens.next match {
    case SingleQuote => ???
    case OpenParen => ???
    case CloseParen => ???
    case InvalidToken(lexeme) => ???
    case expr => expr
  }
}
CODE
))

(slide
  #:title "Handling SingleQuote"
  (mono #:ratio 1.3 #<<CODE
tokens.next match {
  case SingleQuote =>
    if (tokens.hasNext)
      Quote(parse(tokens))
    else
      Err("unexpected <eof>")
}
CODE
))

(slide
  #:title "Handling OpenParen"
  (mono #:ratio 1.3 #<<CODE
tokens.next match {
  case OpenParen =>
    val values = parseExprs(tokens)

    if (tokens.hasNext) {
      tokens.next
      SExpr(values)
    } else Err("missing ')'")
}
CODE
))

(slide
  #:title "Helper definitions"
  (mono #:ratio 1.3 #<<CODE
def parseExprs(
  tokens: BufferedIterator[Token]
): List[Expr] =

  if (tokens.hasNext &&
      tokens.head != CloseParen)
    parse(tokens) :: parseExprs(tokens)
  else
    List.empty
CODE
))

(slide
  #:title "Handling CloseParen, InvalidToken, and everything else"
  (mono #:ratio 1.3 #<<CODE
tokens.next match {
  case InvalidToken(lexeme) =>
    Err(s"unexpected '$lexeme'")

  case CloseParen =>
    Err("unexpected ')'")

  // True, False, Str, Number,
  // Identifier, SExpr, Quote,
  // Lambda, Builtin, Proc, Err
  case expr => expr
}
CODE
))

(slide
  #:title "And now we have an AST"
  (vc-append (* 2 gap-size)
    (mono #<<CODE
parse(tokenize("(+ 21 43)"))
CODE
)
    (arrow gap-size (* pi 1.5))
    (mono #<<CODE
List(OpenParen, Identifier(+),
  Number(21.0), Number(43.0),
  CloseParen)
CODE
)
    (arrow gap-size (* pi 1.5))
    (mono #<<CODE
SExpr(List(Identifier(+),
           Number(21.0),
           Number(43.0)))
CODE
)))

(slide
  #:title "Hey what about Lambda, Proc, and Builtin?"
  (p "You may have noticed that our parser never returns Lambdas, Procs, or
     Builtins. There is a simple answer as to why Procs nor Builtins are
     returned, and that is because those are expression that are meant to only
     be created programmatically, and as such the parser doesn't have to know
     how to parse them.")
  (p "That is not the case of Lambdas."))

(slide
  #:title "This is what is happening right now"
  #:layout 'top
  (blank)
  (blank)
  (vc-append (* 2 gap-size)
    (mono #<<CODE
val code = "(lambda (x) (+ x x))"
parse(tokenize(code))
CODE
)
    (arrow gap-size (* pi 1.5))
    (mono #<<CODE
SExpr(List(
  Identifier(lambda),
  SExpr(List(Identifier(x))),
  SExpr(List(Identifier(+),
             Identifier(x),
             Identifier(x)))))
CODE
)))

(slide
  #:title "But this is what we need"
  #:layout 'top
  (blank)
  (blank)
  (vc-append (* 2 gap-size)
    (mono #<<CODE
val code = "(lambda (x) (+ x x))"
parse(tokenize(code))
CODE
)
    (arrow gap-size (* pi 1.5))
    (mono #<<CODE
Lambda(List(Identifier(x)),
       SExpr(List(Identifier(+),
                  Identifier(x),
                  Identifier(x))))
CODE
)))

(slide
  #:title "From this to that"
  #:layout 'top
  (vc-append (* 2 gap-size)
    (mono #<<CODE
SExpr(List(
  Identifier(lambda),
  SExpr(List(Identifier(x))),
  SExpr(List(Identifier(+),
             Identifier(x),
             Identifier(x)))))
CODE
)
    (arrow gap-size (* pi 1.5))
    (mono #<<CODE
Lambda(List(Identifier(x)),
       SExpr(List(Identifier(+),
                  Identifier(x),
                  Identifier(x))))
CODE
)))

(slide
  #:title "def passLambdas"
  (mono #<<CODE
def passLambdas(expr: Expr): Expr =
  expr match {
    // ...
  }
CODE
))

(slide
  #:title "def passLambdas"
  (mono #<<CODE
expr match {
  case SExpr(Identifier("lambda") ::
             SExpr(args) ::
             body ::
             Nil) => ???
}
CODE
))

(slide
  #:title "def passLambdas"
  (mono #<<CODE
val (params, errs) = ???

if (!errs.isEmpty)
  errs(0)
else
  Lambda(params, body)
CODE
))

(slide
  #:title "def passLambdas"
  (mono #<<CODE
args.foldRight(
  List[Identifier](),
  List[Err]()
) {
  case (curr, (params, errs)) =>
    curr match {
      case id @ Identifier(_) =>
        (id :: params, errs)

      case x => (
        params,
        Err("bad argument") :: errs
      )
    }
}
CODE
))

(slide
  #:title "calling passLambdas"
  (mono #<<CODE
def parse(ts:Iterator[Token]):Expr = {
  val tokens = ts.buffered

  passLambdas(tokens.next match {
    // ...
  })
}
CODE
))

(slide
  #:title "Lambdas!"
  (vc-append (* 2 gap-size)
    (mono #<<CODE
val code = "(lambda (x) (+ x x))"
parse(tokenize(code))
CODE
)
    (arrow gap-size (* pi 1.5))
    (mono #<<CODE
Lambda(List(Identifier(x)),
       SExpr(List(Identifier(+),
                  Identifier(x),
                  Identifier(x))))
CODE
)))

(slide
  #:title "Multiple passes"
  (p "We could employ this method of checking and manipulating an expression
     after it is parsed and before being executed to do many things. In our
     case we are adding a new feature, Lambda expressions, but one could also
     do optimizations, type checking, and other static analysis checks."))

(slide
  #:title "So close"
  (p "So far our interpreter can do a lot. I can parse numbers, booleans,
     strings, s-expression, and it even knows about lambdas! But still, it
     doesn't run any code."))

(slide
  #:layout 'center
  (t "Let’s build an evaluator"))

(slide
  #:title "def evaluate"
  (p "In its simplest form, an evaluator is a function that takes an expression
     and returns another expression. The returned expression can be thought of
     as the simplified version of the original."))

(slide
  #:title "Evaluate this!"
  (table 3
         (list
           (mono "324") (arrow gap-size 0) (mono "324")
           (mono "#t") (arrow gap-size 0) (mono "#t")
           (mono "\"Hello, world.\"") (arrow gap-size 0) (mono "\"Hello, world.\"")
           (mono "(+ 21 43)") (arrow gap-size 0) (mono "64")
           (mono #<<CODE
((lambda (x)
  (add x 20)) 22)
CODE
) (arrow gap-size 0) (mono "42"))

         (list* rc-superimpose
                cc-superimpose
                lc-superimpose)

         cc-superimpose
         (* 2 gap-size)
         gap-size))

(slide
  #:title "def evaluate"
  #:layout 'top
  (mono #:fill #t #<<CODE
def evaluate(expr: Expr, env: Env):
    (Expr, Env) =

  expr match {
    // ...
  }
CODE
))

(slide
  #:title "def evaluate"
  #:layout 'top
  (mono #:fill #t #<<CODE
def evaluate(expr: Expr, env: Env):
    (Expr, Env) =

  expr match {
    case expr @ (True | False
      | _: Str | _: Number
      | _: Quote | _: Lambda
      | _: Builtin | _: Proc
      | _: Err
    ) =>

      (expr, env)
  }
CODE
))

(slide
  #:title "def evaluate"
  #:layout 'top
  (mono #:fill #t #<<CODE
def evaluate(expr: Expr, env: Env):
    (Expr, Env) =

  expr match {
    case id @ Identifier(name) =>
      val err = Err(
        s"unbound variable: $name")

      (env.getOrElse(err), env)
  }
CODE
))

(slide
  #:title "def evaluate"
  #:layout 'top
  (mono #:fill #t #<<CODE
def evaluate(expr: Expr, env: Env):
    (Expr, Env) =

  expr match {
    case SExpr(Nil) =>
      (Err("empty expression"), env)
  }
CODE
))

(slide
  #:title "def evaluate"
  #:layout 'top
  (mono #:fill #t #<<CODE
def evaluate(expr: Expr, env: Env):
    (Expr, Env) =

  expr match {
    case SExpr((id @ Identifier(_))
        :: body) =>

      val (head, _) =
        evaluate(id, env)

      evaluate(
        SExpr(head :: body),
        env)
  }
CODE
))

(slide
  #:title "def evaluate"
  #:layout 'top
  (mono #:fill #t #<<CODE
case SExpr(Lambda(args, body)
    :: values) =>

  val scope = args.zip(values)
    .foldLeft(env) {
      case (env, (arg, value)) =>
        env ++ Map(arg -> value)
    }

  val (ret, _) =
    evaluate(body, scope)

  (ret, env)
CODE
))

(slide
  #:title "def evaluate"
  #:layout 'top
  (mono #:fill #t #<<CODE
def evaluate(expr: Expr, env: Env):
    (Expr, Env) =

  expr match {
    case SExpr(Proc(fn) :: args) =>
      val evaled = args.map {
        arg => evaluate(arg, env)._1
      }

      fn(evaled)
  }
CODE
))

(slide
  #:title "def evaluate"
  #:layout 'top
  (mono #:fill #t #<<CODE
def evaluate(expr: Expr, env: Env):
    (Expr, Env) =

  expr match {
    case SExpr(Builtin(fn) :: args) =>
      fn(args, env)
  }
CODE
))

(slide
  #:title "def evaluate"
  #:layout 'top
  (mono #:fill #t #<<CODE
def evaluate(expr: Expr, env: Env):
    (Expr, Env) =

  expr match {
    case SExpr(head :: _) =>
      val err = Err(
        s"cannot call $head")

      (err, env)
  }
CODE
))

(slide
  #:title "That’s all for evaluate"
  (p "You may have noticed our evaluate function was missing some
     functionality. What happened to conditionals? What about variable
     bindings?"))

(slide
  #:layout 'center
  (t "This is what Proc and Builtin are for"))

(slide
  #:title "Builtin: define"
  (mono #<<CODE
Builtin((args, env) => args match {
  case (id @ Identifier(_))
      :: expr :: Nil =>

    val update = env ++
      Map(id -> expr)

    (expr update)

  case _ =>
    (Err("bad call to define"), env)
})
CODE
))

(slide
  #:title "Builtin: cond"
  (mono #<<CODE
Builtin((args, env) => {
  def aux(conds: List[Expr]): Expr =
    // ...

  (aux(args), env)
}),
CODE
))

(slide
  #:title "Builtin: cond"
  #:layout 'top
  (mono #<<CODE
def aux(conds: List[Expr]): Expr =
  conds match {
    case SExpr(check :: body :: Nil)
        :: rest => ???

    case Nil => SExpr(List.empty)
    case _ => Err("bad syntax. cond")
  }
CODE
))

(slide
  #:title "Builtin: cond"
  #:layout 'top
  (mono #<<CODE
def aux(conds: List[Expr]): Expr =
  conds match {
    case SExpr(check :: body :: Nil)
        :: rest =>

      evaluate(check, env)._1 match {
        case False => aux(rest)
        case _ =>
          evaluate(body, env)._1
      }

    case Nil => SExpr(List.empty)
    case _ => Err("bad syntax. cond")
  }
CODE
))

(slide
  #:title "Builtin: add"
  (mono #<<CODE
Proc((args, env) =>
  (args match {
    case Number(a) :: Number(b)
        :: Nil =>

      Number(a + b)

    case _ => Err("bad call to add")
  }, env))
CODE
))

(slide
  #:layout 'center
  (t "Let’s test it out"))

(slide
  #:title "def run"
  (mono #:fill #t #<<CODE
def run(code: String, env: Env):
    Expr =

  evaluate(parse(tokenize(code)),
    env)._1
CODE
))

(slide
  (vc-append (* 3 gap-size)
    (mono #<<CODE
val code = """
  ((lambda (x) (add x 20)) 22)
"""

val env = Map(
  Identifier("add") -> builtinAdd
)

run(code, env)
CODE
)
    (arrow gap-size (* pi 1.5))
    (mono #<<CODE
Number(42.0)
CODE
)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TODO ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TODO ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(slide
  #:layout 'center
  (titlet "From parsing to interpretation")
  (t "We’ve built a language"))

(slide
  #:title "Resources"
  (unordered
    "https://en.wikipedia.org/wiki/Extended_Backus-Naur_form"
    "https://en.wikipedia.org/wiki/Abstract_syntax_tree"
    "https://en.wikipedia.org/wiki/Multi-pass_compiler"))
