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

(require slideshow/code)
(require slideshow/text)

(current-main-font "Baskerville")
(current-code-font "IBM Plex Mono")
(code-colorize-enabled #f)

(define (unmargin line)
  (let ([parts (string-split line "|")])
    (string-trim (if (= 1 (length parts))
                   (car parts)
                   (car (cdr parts))))))

(define (mono texts #:ratio [ratio 1.1])
  (apply vc-append 1
         (for/list ([line (string-split (string-trim texts) "\n")])
           (with-size ((get-current-code-font-size))
                      (with-font (current-code-font)
                                 (para #:fill? #t
                                       #:width (* ratio (current-para-width))
                                       #:align 'left
                                       line))))))

(define (code line)
  (with-size ((get-current-code-font-size))
             (with-font (current-code-font)
                        (t line))))

(define (p txt)
  (para (string-normalize-spaces txt)))

(slide
  #:layout 'center
  (titlet "From parsing to interpretation")
  (t "Let's build a language"))

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
  (blank)
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
  (t "Let's build an interpreter."))

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
  (item "Parsers")
  (item "Grammars")
  (item "BNF/EBNF")
  (item "Parser generators")
  (item "Recursive descent parsers")
  (item "Top down parser")
  (item "Scope")
  (item "Evaluation"))

(slide
  #:title "Where do we really start?"
  (p "1 - Parse")
  (p "2 - Evaluate"))

(slide
  #:title "This is where we start"
  (p "1 - Define what our language looks like.")
  (p "2 - Tokenize the input into a stream of valid tokens.")
  (p "3 - Take the stream of tokens and compose them into complete expressions.")
  (p "4 - Evaluate the expressions."))

(slide
  #:layout 'center
  (t "Let's define a language."))

(slide
  #:layout 'center
  (t "What can our language do?"))

(slide
  #:title "It can understand numbers"
  #:layout 'center
  (code "7"))

(slide
  #:title "It can understand strings"
  #:layout 'center
  (code "\"Hello, world.\""))

(slide
  #:title "It can understand something is logically true"
  #:layout 'center
  (code "#t"))

(slide
  #:title "It can understand something is logically false"
  #:layout 'center
  (code "#f"))

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
  (code "(* 21 2)"))

(slide
  #:title "It can define functions"
  (code "(lambda (n) (* n 2))"))

(slide
  #:title "It can store all of those values"
  (code "(define double (lambda (n) (* n 2)))"))

(slide
  #:title "It can apply functions to parameters"
  #:layout 'center
  (code "(double 21)"))