#lang racket

;生成一组数字的相互交错集合

(define (remv a ls)
  (cond ((list? a) (remove* a ls))
        (else (remove a ls))))

(define (permute ls)
  (cond ((null? ls) '())
        (else (append (map (λ (x) (append (list (car ls)) (list x))) (remove (car ls) ls))
                      (map (λ (x) (append (list x) (list (car ls)))) (remove (car ls) ls))
                      (permute (cdr ls))))))

(define (permute* ls)
  (define (iter lt lr rem)
    (cond ((null? lr) lt)
          ((null? lt) (iter (map (λ (x) (flatten x)) rem) (cdr lr) '()))
          (else (iter (cdr lt) lr
                      (append rem (map (λ (x) (append (list (car lt)) (list x))) (remv (car lt) ls)))))))
  (iter (permute ls) (cddr ls) '()))

;求因子
(define (gather-div num)
  (letrec
      ((R (λ (div)
            (if (= div num)
                (list div)
                (if (= (remainder num div) 0)
                    (cons div (R (+ div 1)))
                    (R (+ div 1)))))))
    (R 1)))

(define (g-div num)
  (for ([i (range 1 25)])
    (if (= (remainder num i) 0)
        (printf "~a~n" i)
        '())))

;排序两组有序列表
(define (comp ls lt)
    (cond ((null? ls) lt)
          ((null? lt) ls)
          ((<= (car ls) (car lt))
           (cons (car ls)
                 (comp (cdr ls) lt)))
          (else (cons (car lt)
                      (comp ls (cdr lt))))))

;巴塞尔级数，结果可以用(exact->inexact n)来转换成小数
(define (basel n)
  (define (iter cnt res)
    (if (= cnt (+ n 1))
        res
        (iter (+ cnt 1) (+ res (/ 1 (* cnt cnt))))))
  (iter 1 0))
