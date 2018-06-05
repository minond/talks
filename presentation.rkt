#lang slideshow

; ===== Talk Outline =====
;
; - Topic introduction
;   - What is this talk about?
;   - Who is this talk for?
;   - What are we building?
;   - What is an interpreter?
;   - What does our language look like?
;   - Why does our language look the way it does?
; - What are languages?
; - Why one might care about languages?
; - What one might get out of learning about languages?
; - Overview of an interpreter
; - Tokenizer
;   - Tokenizer generators
; - Parser
;   - BNF/EBNF
;   - Parser generators
;   - Top down parsers
;   - Recursive descent parsers
; - Evaluator
;   - Pattern matching in Scala

(require
  slideshow/code
  slideshow/text
  "slideshow.rkt")

(current-main-font "Baskerville")
(current-code-font "IBM Plex Mono")
(code-colorize-enabled #f)

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
  (t "Let’s build an interpreter."))

(slide
  (scale (bitmap (build-path "assets" "interpreter-definitions.png")) .5))

(slide
  #:title "A program that can analyze a program"
  (mono #<<CODE
> 21 * 2
...
CODE
))

(slide
  #:title "A program that can analyze a program"
  (mono #<<CODE
> 21 * 2
< 42
CODE
))

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
  (t "Let’s define a language."))

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
  #:title "It can understand something is logically true"
  #:layout 'center
  (mono "#t"))

(slide
  #:title "It can understand something is logically false"
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
  #:title "It can store all of those values"
  (mono "(define double (lambda (n) (* n 2)))"))

(slide
  #:title "It can apply functions to parameters"
  #:layout 'center
  (mono "(double 21)"))

(slide
  #:layout 'center
  (t "Let’s build a BNF grammar."))

(slide
  #:layout 'center
  (t "Let’s build an EBNF grammar."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TODO ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(slide
  #:layout 'center
  (t "TODO TALK ABOUT EBNF HERE!!!"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TODO ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(slide
  #:title "Numbers"
  (mono #<<CODE
number = [ "-" ] , digit { digit } ;

digit = 0 | 1 | 2 | 3 | 4 | 5 | 6
      | 7 | 8 | 9 ;
CODE
))

(slide
  #:title "Strings"
  (mono #<<CODE
string = '"' , { letter } , '"' ;

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

identifier = identifier ,
   { letter | symbol | digit } ;
CODE
))

(slide
  #:title "S-expressions"
  (mono #<<CODE
atom = identifier | number | boolean ;

exprs = [ "'" ] ( atom | sexpr ) ;

sexpr = "(" { exprs } ")" ;
CODE
))

(slide
  #:title "All together now"
  (mono #<<CODE
main    = { exprs } ;
number  = [ "-" ] , digit { digit } ;
digit   = 0 ... 9 ;
string  = '"' , { letter } , '"' ;
letter  = "A" ... "z" ;
boolean = "#t" | "#f" ;
identifier = identifier ,
        { letter | symbol | digit } ;
symbol  = "<" | ">" | "*" | "+" | "-"
        | "=" | "_" | "/" | "%" ;
atom    = identifier | number
        | boolean ;
exprs   = [ "'" ] ( atom | sexpr ) ;
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
  (t "Let’s build a parser."))

(slide
  #:title "But wait!"
  (p "Actually, let’s take a step back. Characters are hard but what if we had
     ‘words’ instead?"))

(slide
  #:title "A program that can analyze a program"
  (mono #:ratio 1.3 #<<CODE
def tokenize(str: String): Iterator[Token] = {
  val src = str.toList.toIterator.buffered
  for (c <- src if !c.isWhitespace)
    yield c match {
      case '(' => OpenParen
      case ')' => CloseParen
      case '\'' => SingleQuote
      case '"' => ...
      case n if isDigit(n) => ...
      case c if isIdentifier(c) => ...
      case '#' => ...
      case c => ...
    }
}
CODE
))

(slide
  #:title "Resources"
  (unordered
    "https://en.wikipedia.org/wiki/Extended_Backus-Naur_form"
    "https://en.wikipedia.org/wiki/Yacc"
    "https://en.wikipedia.org/wiki/GNU_bison"
    "https://en.wikipedia.org/wiki/ANTLR"))
