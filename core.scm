(define null?
  (lambda (xs)
    (equal? xs (list))))

(define zero?
  (lambda (n)
    (equal? n 0)))

(define map
  (lambda (f xs)
    (cond
      ((null? xs) '())
      (#t (cons (f (car xs)) (map f (cdr xs)))))))

(define reduce
  (lambda (id f xs)
    (cond
      ((null? xs) id)
      (#t (reduce (f id (car xs)) f (cdr xs))))))

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

(define nth
  (lambda (i xs)
    (cond
      ((null? xs) '())
      ((zero? i) (car xs))
      (#t (nth (dec i) (cdr xs))))))

(define double
  (lambda (x)
    (+ x x)))

(define triple
  (lambda (x)
    (+ x x x)))
