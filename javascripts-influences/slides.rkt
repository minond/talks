#lang slideshow

(require
  slideshow/code
  slideshow/text)

(current-main-font "Baskerville")
(current-code-font "IBM Plex Mono")
(code-colorize-enabled #f)

(define (unmargin line)
  (let ([parts (string-split line "|")])
    (string-trim (if (= 1 (length parts))
                   (car parts)
                   (car (cdr parts))))))

(define (mono texts #:ratio [ratio 1] #:fill [fill #f])
  (apply vl-append 1
         (for/list ([line (string-split (string-trim texts) "\n")])
           (with-size (- ((get-current-code-font-size)) 4)
                      (with-font (current-code-font)
                                 (para #:fill? fill
                                       #:width (* ratio (current-para-width))
                                       #:align 'left
                                       line))))))

(define (code str)
  (with-size (- (current-font-size) 6)
             (with-font (current-code-font)
                        (t str))))

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

(slide
  #:layout 'center
  (titlet "Exploring JavaScript's Influences"))
