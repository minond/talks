;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Standard Library Tests
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Core functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(assert (boolean? #f))
(assert (boolean? #t))
(assert (builtin? eval))
(assert (error? (error 'ok)))
(assert (false? #f))
(assert (integer? 123))
(assert (lambda? (lambda () 'ok)))
(assert (list? '()))
(assert (list? '(1 2 3)))
(assert (list? (list 1 2 3)))
(assert (list? (list)))
(assert (null? '()))
(assert (pair? (cons 1 2)))
(assert (quote? 'ok))
(assert (real? 1.2))
(assert (string? "ok"))
(assert (true? #t))
(assert (zero? 0))
(assert (number? -1))
(assert (number? 0))
(assert (number? 1))
(assert (number? 1.0))

(assert-eq '(2 4 6 8) (map double '(1 2 3 4)))
(assert-eq 10 (fold 0 + '(1 2 3 4)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; List and pairs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(assert-eq '(1 2 3) (list 1 2 3))
(assert-eq '(1 2 3) (list (+ 1) (+ 1 1) (+ 1 1 1)))
(assert-eq 1 (first '(1 2 3 4)))
(assert-eq 2 (second '(1 2 3 4)))
(assert-eq 3 (third '(1 2 3 4)))
(assert-eq 4 (length '(1 2 3 4)))
(assert-eq 4 (nth 3 '(1 2 3 4)))
(assert-eq 4 (last '(1 2 3 4)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Comparison
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(assert-eq #t (= 1 1))
(assert-eq #f (= 0 1))
(assert-eq #t (= 0 0))
(assert-eq #f (= "0" 0))
(assert-eq #f (= 0 "0"))
(assert-eq #f (= "0" "0"))
(assert-eq #f (!= 1 1))
(assert-eq #t (!= 0 1))
(assert-eq #f (!= 0 0))
(assert-eq #f (>= 0 1))
(assert-eq #t (>= 1 1))
(assert-eq #t (>= 2 1))
(assert-eq #f (< 2 1))
(assert-eq #f (< 2 2))
(assert-eq #t (< 1 2))
(assert-eq #f (<= 2 1))
(assert-eq #t (<= 2 2))
(assert-eq #t (<= 1 2))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Logic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(assert-eq #t (all? integer? 1 2 3 4 5))
(assert-eq #f (all? integer? 1 2 3 4 5.0))
(assert-eq #t (all? integer?))
(assert-eq #t (or #f #f #t invalid invalid invalid))
(assert-eq #t (or 1 2 3 4))
(assert-eq #f (and #f #f #t))
(assert-eq #t (and #t #t #t))
(assert-eq #t (and 1 2 3 4))
(assert-eq #t (not #f))
(assert-eq #f (not #t))
(assert-eq #f (not '()))
(assert-eq #f (not 0))
(assert-eq #f (not ""))
(assert-eq #f (not 'ok))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Math
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(assert-eq 4 (double 2))
(assert-eq 16 (double 8))
(assert-eq 9 (triple 3))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(assert (ok? 'ok))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(assert-eq 24
  (-> 2
      (double)
      (double)
      (triple)))
