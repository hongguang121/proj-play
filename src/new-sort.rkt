#lang racket

(provide rand-num
         how-long
         mytime)

(define mytime current-inexact-milliseconds)

;计时器
(define (how-long fn . para)
  (let ((start (mytime)))
    (apply fn para)
    (- (mytime) start)))

(define (hl)
  (λ args
    (let ((ls args))
      (letrec ((R (λ (ls rem)
                    (if (null? ls)
                        rem
                        (R (cdr ls) (append rem (list (car ls))))))))
        (R ls '())))))

;自动生成随机数字，并输出到文件 n = 输出数字个数 r = 小于r的随机数
(define (rand-num n r)
  (letrec ((R (lambda (cnt)
                (if (= cnt n)
                    '()
                    (cons (random r)
                          (R (+ cnt 1)))))))
    (R 0)))

;(define out (open-output-file "f:/num.txt" #:exists 'replace))
;(write (rand-num 1000000 10000000) out)
;(close-output-port out)

;(define ls (file->list "f:/num.txt"))

(define (cut ls start end)
  (cond ((= start end) '())
        (else (cons (list-ref ls start)
                    (cut ls (+ start 1) end)))))

;make-matrix (make-list 10 (make-list 10 0))

;用编程珠玑中的方法排序n个数
;生成表，各位置为0
(define (n-list n)
  (make-list n 0))

;将n个数的随机序列插入ls = (n-list n)
(define (insert ln ls)  
    (cond ((null? ln) ls)
          (else (insert (cdr ln) (list-set ls (car ln) (car ln))))))

;随机生成一组小于num的n个数字列表
(define (generate num n)  
  (if (= n 0)
      '()
      (cons (random num)
            (generate num (- n 1)))))

;插入排序
(define (insert-sort ls)
  (define (iter ls ln rem)
    (cond ((null? ls) ln)
          ((null? ln)
           (iter (cdr ls) (append rem (list (car ls))) '()))
          ((< (car ls) (car ln))
           (iter (cdr ls) (append rem (cons (car ls) ln)) '()))
          (else (iter ls (cdr ln) (append rem (list (car ln)))))))
  (iter (cdr ls) (list (car ls)) '()))

(define (insert-sort+ ls)
  (let loop ((ls ls)             
             (res '())
             (rem '()))
    (cond ((null? ls) res)
          ((null? res)
           (loop (cdr ls)                 
                 (append (reverse rem) (list (car ls)))
                 '()))                 
          ((<= (car ls) (car res))           
           (loop (cdr ls)
                 (append (reverse rem) (list (car ls)) res)
                 '()))          
          (else (loop ls (cdr res) (cons (car res) rem))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (insert-s ls)
  (if (= (length ls) 1)
      ls
      (let loop ((a (car ls))
                 (ls (insert-s (cdr ls))));通过递归保存列表状态
        (cond ((null? ls) (list a))
              ((< a (car ls)) (cons a ls))
              (else (cons (car ls) (loop a (cdr ls))))))))

;冒泡排序
(define (bubble-sort ls)
  (cond ((null? ls) '())
        (else (cons (apply min ls)
                    (bubble-sort (remv (apply min ls) ls))))))

;快速排序
(define (q-sort s)
  (if (< (length s) 2)
      s
      (append
       (q-sort (filter 
                (lambda (x)
                  (< x (last s)))
                s))
       (filter (lambda (x)
                 (= x (last s)))
               s)
       (q-sort (filter 
                (lambda (x)
                  (> x (last s)))
                s)))))

;找出马(走日)从(x,y)跳到棋盘右下角(n,n)的一条路径
;删除复合列表中的数据
(define (remove* ls lls)
  (cond ((null? lls) '())
        ((and (= (car ls) (caar lls))
              (= (cadr ls) (cadar lls)))
         (remove ls (cdr lls)))
        (else (cons (car lls)
                    (remove ls (cdr lls))))))

;比较列表是否相等
(define (list=? ls lt)
  (cond ((null? ls) (null? lt))
        ((null? lt) (null? ls))
        ((eq? (car ls) (car lt))
         (list=? (cdr ls) (cdr lt)))
        (else #f)))

;确定下一步的坐标 coordinate = '(x y) x:行号 y:列号 direction = 0 ~ 7 从左上角开始
(define (next coor dire)
  (cond ((= dire 0) (list (- (car coor) 1) (- (cadr coor) 2)))
        ((= dire 1) (list (- (car coor) 2) (- (cadr coor) 1)))
        ((= dire 2) (list (- (car coor) 2) (+ (cadr coor) 1)))
        ((= dire 3) (list (- (car coor) 1) (+ (cadr coor) 2)))
        ((= dire 4) (list (+ (car coor) 1) (- (cadr coor) 2)))
        ((= dire 5) (list (+ (car coor) 2) (- (cadr coor) 1)))
        ((= dire 6) (list (+ (car coor) 2) (+ (cadr coor) 1)))
        ((= dire 7) (list (+ (car coor) 1) (+ (cadr coor) 2)))))

;验证下一步是否在棋盘坐标内 ls = (next coor dire) coor = 棋盘从(0 0)开始，coor是最右下角的坐标 8*8的棋盘coor = 7
(define (can? ls coor)
  (and (>= (car ls) 0)
       (>= (cadr ls) 0)
       (<= (car ls) coor)
       (<= (cadr ls) coor)))

;随机走一条合法的深度为10的路径 ls = 初始坐标
(define (root ls)
  (define (iter cnt ln rem)
    (let ((nxt (random 8)))
      (cond ((= cnt 10) (reverse rem))
            ((can? (next ln nxt) 7)
             (iter (+ cnt 1) (next ln nxt) (cons (next ln nxt) rem)))
            (else (iter cnt ln rem)))))
  (iter 0 ls (list ls)))

;避免走回头路
(define route '((0 . 7) (1 . 6) (2 . 5) (3 . 4) (4 . 3) (5 . 2) (6 . 1) (7 . 0)))

;方向索引，共8个方向
(define direction '(0 1 2 3 4 5 6 7))

;根据坐标ls筛选出安全方向 coor = 正方形棋盘边长，从0开始
(define (safe-dire ls coor)
  (cond ((and (= (car ls) 0) (< (cadr ls) 2)) '(5 6 7))
        ((and (= (car ls) 0) (> (cadr ls) (- coor 2))) '(4 5 6))
        ((= (car ls) 0) '(4 5 6 7))
        ((and (= (car ls) coor) (< (cadr ls) 2)) '(1 2 3))
        ((and (= (car ls) coor) (> (cadr ls) (- coor 2))) '(0 1 2))
        ((= (car ls) coor) '(0 1 2 3))
        ((and (= (cadr ls) 0) (< (car ls) 2)) '(3 6 7))
        ((and (= (cadr ls) 0) (> (car ls) (- coor 2))) '(2 3 7))
        ((= (cadr ls) 0) '(2 3 6 7))
        ((and (= (cadr ls) coor) (< (car ls) 2)) '(0 4 5))
        ((and (= (cadr ls) coor) (> (car ls) (- coor 2))) '(0 1 4))
        ((= (cadr ls) coor) '(0 1 4 5))
        ))

;快速逼近棋盘右下角
;(define (approach ls coor))

;解决问题 ls = 初始坐标 coor = 正方形棋盘的边长，从0开始 8*8 = 7
(define (slove ls coor)
  (let ((dire (random 8)))
    (define (iter ln dire rem cnt)
      (let ((dire (list-ref (remv (cdr (assoc dire route)) direction) (random 7))))
        (cond ((list=? ln (list coor coor))
               (reverse (cons ln rem)))
              ((= cnt 500) (reverse rem))
              ((can? ln coor)
               (iter (next ln dire) dire (cons ln rem) (+ cnt 1)))
              (else (iter (next (car rem) dire) dire rem cnt)))))
    (iter (next ls dire) dire (list ls) 0)))
  
;二分查找
(define (b-search ls num)
  (define (iter id start end)
    (cond ((= num (list-ref ls id))
           (format "~a 在列表id: ~a 处" num id))
          ((< num (list-ref ls id))
           (iter (ceiling (/ (+ start id) 2)) start id))
          (else (iter (ceiling (/ (+ id end) 2)) id end))))
  (iter (ceiling (/ (length ls) 2)) 0 (length ls)))

;merge-sort
(define (merge left right)
  (cond ((null? left) right)
        ((null? right) left)
        (else 
         (let ((l (car left))
               (r (car right)))
           (cond ((<= l r) (cons l (merge (cdr left) right)))
                 ((> l r) (cons r (merge left (cdr right)))))))))

;T(n)=2T(n/2)+Θ(n)
(define (merge-sort ls)
  (cond ((= (length ls) 1) ls)
        (else (let* ((mid (round (/ (length ls) 2)))
                     (left (take ls mid))
                     (right (drop ls mid)))
              (merge (merge-sort left)
                     (merge-sort right))))))

;分解一个自然数
(define (bit num)
  (define (iter const div i res)
    (cond ((= i 0) res)
          (else (iter const
                      (* div const)
                      (floor (/ num div))
                      (cons (remainder i const) res)))))
  (iter 10 10 num '()))

;补齐一组长度不一的数字列表，以最长位为主，不足位补0;
;lls = (define lls (filter-not (λ (x) (eq? x '())) (map bit (rand-num 100 100))))
;maxlen = (apply max (map length lls))
(define (same-len lls maxlen) 
    (map (λ (x) (if (< (length x) maxlen) (add-zero x (- maxlen (length x))) x)) lls))

;补零 给ls加n个0
(define (add-zero ls n)
  (if (= n 0)
      ls
      (cons 0 (add-zero ls (- n 1)))))

;基数排序 (radix (same-len lls 2)) ;lls是2位数
(define (radix lls)
  (define (iter len ls)
    (cond ((< len 0) ls)
          (else (iter (- len 1) (sort ls #:key (λ (x) (list-ref x len)) <)))))
  (iter (- (length (car lls)) 1) lls))

;将基数排序结果列表恢复为数字序列,n是最高位数
(define (to-numb ls n)
  (define (iter ls lt cnt res rem)
    (cond ((null? lt) (iter (cdr ls) (car ls) (- n 1) 0 (cons res rem)))
          ((null? ls) (reverse rem))          
          (else (iter ls (cdr lt) (- cnt 1) (+ (* (expt 10 cnt) (car lt)) res) rem))))
  (iter ls (car ls) (- n 1) 0 '()))