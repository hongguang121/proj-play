#lang racket

(define (mytime) (current-milliseconds))

(define (test-iter)
  (let loop ((start (mytime))
             (id 0))
    (if (> (- (mytime) start) 1000)
        id
        (loop start (add1 id)))))

(command-line
 ;#:program "test-iter"
 #:args (proc)
 ;((eval (string->symbol proc))))
 (cond ((string=? proc "test-iter")
        (test-iter))))