#lang racket

(define algo
  (lambda (symb)
    (lambda args
      (let ([ls args])
        (letrec
            ((R (lambda (ls res)
                  (cond ((null? ls) res)
                        (else (R (cdr ls) (symb (car ls) res)))))))
          (R ls 1))))))

(define fact
  (letrec
      ((fact
        (lambda (n)
          (if (= n 0)
              1
              (* n (fact (- n 1)))))))
    fact))

;在Y组合子中fact必须是一个procedure，不可以是一个symbol

(define (sqrt+ n)
  (define (enough? res)
    (if (< (abs (- n (* res res))) 0.0000000001)
        #t
        #f))
  (define (iter guess res)
    (cond ((enough? res) res)
          (else (iter (/ (+ guess res) 2) (/ n (/ (+ guess res) 2))))))
  (iter 1 (/ n 1)))

;函数运行在定义它的作用域中，而非调用它的作用域
(let ((a 1)
      (b 2))
  (define (plus x)
    (+ a b x))
  (let ((a 3)
        (b 4))
    (plus 3)))

