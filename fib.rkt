#lang racket

(define (fib n)
    (if (< n 2)
        n
        (+ (fib (- n 1))
           (fib (- n 2)))))

(command-line
 #:program "fib performance"
 #:args (n)
 (time (fib (string->number n))))