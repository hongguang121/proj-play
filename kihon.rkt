#lang racket

(define (sum ls)
    (if (null? ls)
        0
        (+ (car ls) (sum (cdr ls)))))

(define (max ls)
    (let loop ((val (car ls))
               (ls (cdr ls)))
      (if (null? ls)
          val
          (if (> val (car ls))
              (loop val (cdr ls))
              (loop (car ls) (cdr ls))))))

(define ls '(5 4 3 2 6 8))

;;js: const sum = ls.reduce((sum, n) => sum + n, 0)
;;js: const max = ls.reduce((max, val) => (val > max) ? val : max, 0)
;;js: reduce's 1st arg: 合并 , 2nd arg: ls值 , => 1st exp: 递归计算合并值 , 2nd exp: 初始化合并值

(flatten (map (λ (s) (regexp-match* #rx"." s)) '("Hello" "World")))

