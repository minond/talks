#lang slideshow

(provide
  unmargin
  mono
  ordered
  unordered
  p)

(require
  slideshow/code
  slideshow/text)

(define (unmargin line)
  (let ([parts (string-split line "|")])
    (string-trim (if (= 1 (length parts))
                   (car parts)
                   (car (cdr parts))))))

(define (mono texts)
  (apply vl-append 1
         (for/list ([line (string-split (string-trim texts) "\n")])
           (with-size ((get-current-code-font-size))
                      (with-font (current-code-font)
                                 (para #:fill? #f
                                       #:width (current-para-width)
                                       #:align 'left
                                       line))))))

(define (ordered . items)
  (apply vl-append gap-size
         (for/list ([x items]
                    [n (range 1 (+ 1 (length items)))])
           (item
             #:gap-size 0
             #:bullet (t (format "~s - " n))
             x))))

(define (unordered . items)
  (apply vl-append gap-size
         (for/list ([x items])
           (item x))))

(define (p txt)
  (para (string-normalize-spaces txt)))
