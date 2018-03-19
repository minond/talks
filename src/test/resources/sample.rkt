;; This is not actually Racket code but rather a playground for what the Lisp
;; looks like.

; There's variables, strings, numbers, booleans, and lists
(define name "Language1")
(define x 10)
(define no #f)
(define foods '(banana apple orange))

; And functions, with parameters
(define id
  (lambda (x) x))

; Conditionals
(cond
  ((= 10 x) (printfln "x = 10"))
  (#t (printfln "x = ???")))

; Arithmetic
(printfln "x = %i" (- x 1))
(printfln "x = %i" (+ x 1))

; Recursion
(define count
  (lambda (xs)
    (cond
      ((null? xs) 0)
      (#t (+ 1 (count (cdr xs)))))))

(printfln "(count %s) => %i" foods (count foods))

(define max
  (lambda (xs)
    (cond
      ((null? xs) (error "Cannot call max on a empty list"))
      ((null? (cdr xs)) (car xs))
      (#t (begin (define y (max (cdr xs)))
                 (define x (car xs))
                 (cond
                   ((> x y) x)
                   (#t y)))))))

; Higher-order functions
(define reduce
  (lambda (f acc xs)
    (cond
      ((null? xs) acc)
      (#t (reduce f (f (car xs) acc) (cdr xs))))))

; And lexical scoping
(define count-and-sum
  (lambda (xs total)
    (+ total (count xs))))

(define list-of-lists
  '((1 2 3) (4 5 6) (7 8 9)))

(printfln "(reduce %s 0 %s) => %i" count-and-sum list-of-lists
          (reduce count-and-sum 0 list-of-lists))
