#lang racket

;(define (guess num)
;  (define (guess-in n g)
;    (cond ((< n g) (display "bigger ") (guess-in (+ n 1) g))
;          ((> n g) (display "smaller ") (guess-in (- n 1) g))
;          ((= n g) (display "right ") g)))
;  (guess-in num (truncate (random 100))))


(define g (round (* (random) 100)))

(define (guess x)
  (if (= x g)
      "bingo!"
      (if (< x g)
          "Small, guess again!"
          "big, guess again!")))