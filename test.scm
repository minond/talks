(assert-eq 10 (fold 0 + '(1 2 3 4)))
(assert-eq '(2 4 6 8) (map double '(1 2 3 4)))

(assert-eq '(1 2 3) (list 1 2 3))
(assert-eq '(1 2 3) (list (+ 1) (+ 1 1) (+ 1 1 1)))
