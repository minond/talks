;; Core functions

(define thunk/equal?
  (lambda (a)
    (lambda (b)
      (equal? a b))))

(define thunk/type/equal?
  (lambda (t)
    (lambda (x)
      (equal? (type/name x) t))))

(define boolean? (thunk/type/equal? "boolean"))
(define builtin? (thunk/type/equal? "builtin"))
(define error? (thunk/type/equal? "error"))
(define false? (thunk/equal? #f))
(define integer? (thunk/type/equal? "integer"))
(define lambda? (thunk/type/equal? "lambda"))
(define list? (thunk/type/equal? "sexpr"))
(define null? (thunk/equal? (list)))
(define pair? (thunk/type/equal? "pair"))
(define quote? (thunk/type/equal? "quote"))
(define real? (thunk/type/equal? "real"))
(define string? (thunk/type/equal? "string"))
(define true? (thunk/equal? #t))
(define zero? (thunk/equal? 0))

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
