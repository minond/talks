;;; Standard Library

;; Core

(define null?
  (lambda (xs)
    (equal? xs (list))))

(define zero?
  (lambda (n)
    (equal? n 0)))

(define builtin?
  (lambda (x)
    (equal? (type/name x) "builtin")))

(define error?
  (lambda (x)
    (equal? (type/name x) "error")))

(define true?
  (lambda (b)
    (equal? b #t)))

(define false?
  (lambda (b)
    (equal? b #f)))

(define boolean?
  (lambda (x)
    (equal? (type/name x) "boolean")))

(define integer?
  (lambda (x)
    (equal? (type/name x) "integer")))

(define lambda?
  (lambda (x)
    (equal? (type/name x) "lambda")))

(define pair?
  (lambda (x)
    (equal? (type/name x) "pair")))

(define quote?
  (lambda (x)
    (equal? (type/name x) "quote")))

(define real?
  (lambda (x)
    (equal? (type/name x) "real")))

(define list?
  (lambda (x)
    (equal? (type/name x) "sexpr")))

(define string?
  (lambda (x)
    (equal? (type/name x) "string")))

(define map
  (lambda (f xs)
    (cond
      ((null? xs) '())
      (#t (cons (f (car xs)) (map f (cdr xs)))))))

(define fold
  (lambda (id f xs)
    (cond
      ((null? xs) id)
      (#t (fold (f id (car xs)) f (cdr xs))))))

(define filter
  (lambda (f xs)
    (cond
      ((null? xs) '())
      ((f (car xs))
       (cons (car xs)
             (filter f (cdr xs))))
      (#t (filter f (cdr xs))))))

;; List and Pair functions

(define list
  (lambda (. xs) xs))

(define first
  (lambda (xs)
    (cond
      ((null? xs) (error "first expects a non-empty list"))
      (#t (car xs)))))

(define second
  (lambda (xs)
    (cond
      ((null? xs) (error "second expects a two-item list"))
      ((null? (cdr xs)) (error "second expects a two-item list"))
      (#t (car (cdr xs))))))

(define third
  (lambda (xs)
    (cond
      ((null? xs) (error "third expects a three-item list"))
      ((null? (cdr xs)) (error "third expects a three-item list"))
      ((null? (cdr (cdr xs))) (error "third expects a three-item list"))
      (#t (car (cdr (cdr xs)))))))

(define length
  (lambda (xs)
    (cond
      ((null? xs) 0)
      (#t (+ 1 (length (cdr xs)))))))

(define nth
  (lambda (i xs)
    (cond
      ((null? xs) '())
      ((zero? i) (car xs))
      (#t (nth (dec i) (cdr xs))))))

;; Math functions

(define double
  (lambda (x)
    (+ x x)))

(define triple
  (lambda (x)
    (+ x x x)))

;; Testing functions

(define assert
  (lambda (ret)
    (cond
      ((true? ret) 'ok)
      (#t (error "Assertion error")))))

(define assert-eq
  (lambda (expected ret)
    (assert (equal? expected ret))))
