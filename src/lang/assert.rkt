;; Testing functions

(define ok? (thunk/equal? 'ok))

(define assert
  (lambda (ret)
    (cond
      ((true? ret) 'ok)
      (#t (error "Assertion error")))))

(define assert-eq
  (lambda (expected ret)
    (assert (equal? expected ret))))
