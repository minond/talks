;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Standard Library
;;
;; 1. Internal functions
;; 2. Core functions
;; 3. List and pairs
;; 4. Comparison
;; 5. Logic
;; 6. Math
;; 7. Test helpers
;; 8. Macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define thunk/equal?
  (lambda (a)
    (lambda (b)
      (equal? a b))))

(define thunk/type/equal?
  (lambda (t)
    (lambda (x)
      (equal? (type/name x) t))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Core functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define boolean? (thunk/type/equal? 'boolean))
(define builtin? (thunk/type/equal? 'builtin))
(define error? (thunk/type/equal? 'error))
(define false? (thunk/equal? #f))
(define integer? (thunk/type/equal? 'integer))
(define lambda? (thunk/type/equal? 'lambda))
(define list? (thunk/type/equal? 'sexpr))
(define pair? (thunk/type/equal? 'pair))
(define quote? (thunk/type/equal? 'quote))
(define real? (thunk/type/equal? 'real))
(define string? (thunk/type/equal? 'string))
(define true? (thunk/equal? #t))
(define zero? (thunk/equal? 0))

(define number?
  (lambda (x)
    (or (integer? x) (real? x))))

(define null?
  (lambda (xs)
    (equal? (list) xs)))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; List and pairs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(define last
  (lambda (xs)
    (nth (dec (length xs)) xs)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Comparison
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define =
  (lambda (a b)
    (and (all? number? a b)
         (equal? a b))))

(define !=
  (lambda (a b)
    (not (= a b))))

(define >=
  (lambda (a b)
    (or (= a b)
        (> a b))))

(define <
  (lambda (a b)
    (not (>= a b))))

(define <=
  (lambda (a b)
    (or (= a b)
        (< a b))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Logic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define all?
  (lambda (f . xs)
    (let* ((aux (lambda (xs)
                  (cond
                    ((null? xs) #t)
                    ((f (car xs)) (aux (cdr xs)))
                    (#t #f)))))
      (aux xs))))

(define and
  (lambda :lazy (. checks)
    (let* ((aux (lambda (checks)
                  (cond
                    ((null? checks) #t)
                    ((false? (eval (car checks))) #f)
                    (#t (aux (cdr checks)))))))
      (aux checks))))

(define or
  (lambda :lazy (. checks)
    (let* ((aux (lambda (checks)
                  (cond
                    ((null? checks) #f)
                    ((false? (eval (car checks))) (aux (cdr checks)))
                    (#t #t)))))
      (aux checks))))

(define not
  (lambda (x)
    (if (false? x) #t #f)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Math
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define +
  (lambda (. xs)
    (fold 0 add xs)))

(define *
  (lambda (. xs)
    (fold 1 mult xs)))

(define -
  (lambda (. xs)
    (cond
      ((equal? 0 (length xs)) 0)
      ((equal? 1 (length xs)) (* -1 (car xs)))
      (#t (fold (car xs)
                sub
                (cdr xs))))))

(define sub
  (lambda (a b)
    (add a (mult -1 b))))

(define inc
  (lambda (x)
    (add x 1)))

(define dec
  (lambda (x)
    (sub x 1)))

(define double
  (lambda (x)
    (+ x x)))

(define triple
  (lambda (x)
    (+ x x x)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ok? (thunk/equal? 'ok))

; (define assert
;   (lambda (fn . args)
;     (map eval args)))

(define assert
  (lambda (fn . args)
    (if (apply (eval fn) (map eval args))
      (begin (printf "pass: (apply %s '%s)" fn args)
             (newline))
      (begin (printf "Assertion error!")
             (newline)
             (printf "fail (apply %s '%s)" fn args)
             (newline)
             (halt "Assertion error.")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Clojure's thread-first macro
; https://clojure.org/guides/threading_macros
(define ->
  (lambda :lazy (arg . fns)
    (cond
      ((null? fns) arg)
      (#t (->lambda arg fns)))))

(define ->lambda
  (lambda (arg fns)
    (cond
      ((null? fns) arg)
      (#t
       (->lambda (eval (cons (car (car fns))
                             (cons arg (cdr (car fns)))))
                 (cdr fns))))))

(define if
  (lambda :lazy (check pass fail)
    (cond
      ((false? (eval check)) (eval fail))
      (#t (eval pass)))))
