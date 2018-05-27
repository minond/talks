#lang slideshow

(require slideshow/code)
(require slideshow/text)

(current-main-font "Baskerville")
(current-code-font "IBM Plex Mono")
(code-colorize-enabled #f)

; Outline:
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

(define (unmargin line)
  (let ([parts (string-split line "|")])
    (string-trim (if (= 1 (length parts))
                   (car parts)
                   (car (cdr parts))))))

(define (mono texts)
  (apply vc-append 1
         (for/list ([line (string-split (string-trim texts) "\n")])
           (with-size ((get-current-code-font-size))
                      (with-font (current-code-font)
                                 (para #:fill? #t
                                       #:width (* 1.1 (current-para-width))
                                       #:align 'left
                                       line))))))

(slide
  #:layout 'center
  (titlet "From parsing to interpretation")
  (t "Let's build a language"))

(slide
  #:title "Abstract"
  (para "An understanding of program evaluation can be a truly enlightening thing for a programmer. By breaking down the process to its essence and implementing the parsing and interpretation steps for a Lisp, we will explore both the interesting and oddly simple algorithms employed in making a language.")
  (para "Our Lisp will have familiar characteristics like variables, higher-order functions, conditionals, and lexical scope."))

(slide
  #:title "Scala Code"
  (mono #<<EOL
"cdr" -> Builtin({ (args, env) =>
  safeEval(args, env) match {
    case SExpr(_ :: tail) :: Nil =>
      SExpr(tail)

    case Pair(_, tail) :: Nil => tail
    case _ :: Nil => Error(...)
    case _ => Error(...)
  }
})
EOL
))

(slide
  #:title "Lisp Code"
  (mono #<<CODE
(define -
  (lambda (. xs)
    (cond
      ((equal? 0 (length xs)) 0)
      ((equal? 1 (length xs))
       (* -1 (car xs)))
      (#t (fold (car xs)
                sub
                (cdr xs))))))
CODE
))
