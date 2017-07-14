#lang racket

;偏函数
(define (add n)
    (λ args
      (let ((ls args))
        (letrec
            ((R (λ (ls)
                  (if (null? ls)
                      n
                      (+ (car ls)
                         (R (cdr ls))))
                  )))
          (R ls)))))
      
        