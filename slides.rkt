#lang slideshow

(current-main-font "Baskerville")

(slide
  #:layout 'center

  (titlet "From parsing to interpretation")
  (t "Let's build a language"))

(slide
  #:title "Abstract"
  (para "An understanding of program evaluation can be a truly enlightening thing for a programmer. By breaking down the (or, a part of) process to its essence and implement, from scratch, the parsing and interpretation steps for a Lisp we'll explore both the interesting and oddly simple algorithms employed in making a language.")
  (para "Our Lisp won't be feature rich, since we only have 50 minutes after all, but it will have familiar characteristics like higher-order functions, loops, and lexical scope."))
