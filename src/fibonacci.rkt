#lang racket

(provide fibonacci)

(define (fibonacci n)  
  (if (< n 2)
      n
      (+ (fibonacci (- n 1))
         (fibonacci (- n 2)))))

;(time (fibonacci 45)) ;作为可执行文件

(define (mul-fib)
  (let ((f1 (future (λ () (fibonacci 40))))
        (f2 (future (λ () (fibonacci 40))))
        (f3 (future (λ () (fibonacci 40))))
        (f4 (future (λ () (fibonacci 40))))
        (f5 (future (λ () (fibonacci 40))))
        (f6 (future (λ () (fibonacci 40)))))
    (and (touch f1)
         (touch f2)
         ;(touch f3)
         ;(touch f4)
         ;(touch f5)
         (touch f6))))

;六路并行cpu time: 79390 real time: 13423 gc time: 0
;delay可以缓存计算值

;命令行执行
(command-line
 #:program "fibonacci"
 #:args (arg)
 (time (fibonacci (string->number arg))))

