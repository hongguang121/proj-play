#lang racket


(define s '((2 x) (3 y) (1 z)))
(map (lambda (x) (assoc x s)) (sort (map (lambda (x) (car x)) s) <))

(define (power a b)
  (if (< b 0) 
      (/ 1 (power-iter 1 a b))
      (power-iter 1 a b)))

(define (power-iter n a b)
  (if (< (abs b) 1)
      n
      (power-iter (* n a) a (- (abs b) 1))))

;删除表中元素
(define (rember x lst)
  (cond ((null? lst) '())
        ((equal? x (car lst)) (cdr lst))
        (else (cons (car lst)
                    (rember x (cdr lst))))))

;按b进制取n的各个位
(define (mod n b)
  (define (iter b m q r) ;m:余数 q:商 b:基数 r:结果
    (cond ((= q 0) r)
          (else (iter b
                      (remainder q b)
                      (quotient q b)
                      (cons (remainder q b) r)))))
  (iter b 0 n '()))

;生成一列数字的各个位
(define (gen arr)
  (cond ((null? arr) '())
        (else (cons (reverse (mod (car arr) 10))
                    (gen (cdr arr))))))

;求一组列表中最长列的长度
(define (lenst lst)
  (apply max (map (λ (x) (length x)) lst)))

;对不足最长位(lens)的补零 :通过修改(find-id)后，已经不需要这么做。切记，当陷入困境时一定不要忘记换一种想法来实现。
(define (add-zero lst lens)
  (define (iter lst ls cnt len res)
    (cond ((and (= cnt (car ls)) (= len (length lst))) '())
          ((= cnt (car ls)) (cons res (iter (cdr lst) (cdr ls) 0 (+ len 1) (car (cdr lst)))))
          (else (iter lst ls (+ cnt 1) len (append res '(0))))))
  (iter lst (map (λ (x) (- lens x)) (map (λ (x) (length x)) lst)) 0 1 (car lst)))
  

;按下标在表中取值
(define (find-id id lst)
  (cond ((not (< id (length lst))) 0)
        ((= id 0) (car lst))
        (else (find-id (- id 1) (cdr lst)))))

;按值查找
(define (find-val x lst)
  (cond ((null? lst) '())
        ((= x (car lst)) (cons (car lst)
                               (find-val x (cdr lst))))
        (else (find-val x (cdr lst)))))

;x:数值 id：个十百千…位 
(define (find-list x id lst)
  (cond ((null? lst) '())
        ((= x (find-id id (car lst)))
         (car lst))
        (else (find-list x id (cdr lst)))))

;(gen '(256 768 128 64 1024 8 2 16 32))

;转换数字列表为数字
(define (list->num lst base)
  (define (iter pow id lt sum)
    (cond ((= id (length lst)) '())
          ((null? lt) (cons sum (iter 0 (+ id 1) (find-id (+ id 1) lst) 0)))
          (else (iter (+ pow 1) id (cdr lt) (+ sum (* (car lt) (power base pow)))))))
  (iter 0 0 (find-id 0 lst) 0))

;基数排序
(define (f-sort gen-arr) ;(gen arr)
  (define (iter id len srr rem res) ;id:从个位开始 len:最长位 srr:位排序 rem:新数组 res:一轮结果
    (cond ((null? srr)
           (cond ((= id (- len 1)) (list->num (reverse res) 10))
                 (else (iter (+ id 1) len 
                             (sort (map (λ (x) (find-id (+ id 1) x)) res) <)
                             (reverse res) '()))))
          (else (iter id len
                      (cdr srr)
                      (rember (find-list (car srr) id rem) rem)
                      (cons (find-list (car srr) id rem) res)))))
  (iter 0 (lenst gen-arr) (sort (map (λ (x) (find-id 0 x)) gen-arr) <) gen-arr '()))


