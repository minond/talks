#lang slideshow

(current-main-font "Baskerville")

(slide
  #:layout 'center

  (titlet "From parsing to interpretation")
  (t "Let's build a language"))

(slide
  #:title "Abstract"
  (para "An understanding of program evaluation can be a truly enlightening thing for a programmer. By breaking down the process to its essence and implementing the parsing and interpretation steps for a Lisp, we will explore both the interesting and oddly simple algorithms employed in making a language.")
  (para "Our Lisp will have familiar characteristics like variables, higher-order functions, conditionals, and lexical scope."))
