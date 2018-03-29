#lang slideshow

(require slideshow/code)
(require slideshow/text)

(current-main-font "Baskerville")
(current-code-font "IBM Plex Mono")
(code-colorize-enabled #f)

(define (mono code)
  (text code
        (list* (current-code-font)) (current-font-size)))

(slide
  #:layout 'center
  (titlet "From parsing to interpretation")
  (t "Let's build a language"))

(slide
  #:title "Abstract"
  (para "An understanding of program evaluation can be a truly enlightening thing for a programmer. By breaking down the process to its essence and implementing the parsing and interpretation steps for a Lisp, we will explore both the interesting and oddly simple algorithms employed in making a language.")
  (para "Our Lisp will have familiar characteristics like variables, higher-order functions, conditionals, and lexical scope."))

(slide
  #:title "Code"
  (code
    (define ->
      (lambda :lazy (arg . fns)
        (cond
          ((null? fns) arg)
          (#t (->lambda arg fns)))))))

(slide
  #:title "Code"
  (code
    _import java.nio.charset.Charset
    _import java.nio.file.{Files, Paths}
    _import scala.util.{Try, Failure, Success}))

(slide
  #:title "Code"
  #:gap-size 1

  ; (para (mono "one")
  ;       (blank-line)
  ;       (mono "two")
  ;       (blank-line)
  ;       (mono "three"))

  (mono "one")
  (mono "two")
  (mono "three")

  ; (mono "one")
  ; (blank-line)
  ; (mono "two")
  ; (blank-line)
  ; (mono "three")
  ;
  ; (mono
  ;   (string-join '("one"
  ;                  "two")))
  )
