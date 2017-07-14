#lang racket

;如果要求值一个S-Expression，尤其是s-exp是一个(read)后的函数名，则需要用((eval s-exp) args)函数来求值
;在函数名成为引用的时候'(fun args)可以通过解引用来求值`,(fun args)

;从文件导入
(define s03 (file->string "f:/ssq/2003.txt"))
(define s04 (file->string "f:/ssq/2004.txt"))
(define s05 (file->string "f:/ssq/2005.txt"))
(define s06 (file->string "f:/ssq/2006.txt"))
(define s07 (file->string "f:/ssq/2007.txt"))
(define s08 (file->string "f:/ssq/2008.txt"))
(define s09 (file->string "f:/ssq/2009.txt"))
(define s10 (file->string "f:/ssq/2010.txt"))
(define s11 (file->string "f:/ssq/2011.txt"))
(define s12 (file->string "f:/ssq/2012.txt"))
(define s13 (file->string "f:/ssq/2013.txt"))
(define s14 (file->string "f:/ssq/2014.txt"))
(define s15 (file->string "f:/ssq/2015.txt"))
(define s16 (file->string "f:/ssq/2016.txt"))
(define linked (file->list "f:/ssq/linked.txt"))
(define link-comb (file->list "f:/ssq/link-comb.txt"))
(define sort-link-comb (file->list "f:/ssq/sort-link-comb.txt"))


(define str
  (string-append s15 s14 s13 s12 s11 s10 s09 s08 s07 s06 s05 s04 s03))

(define (to-string str-ref)
  (list->string (list str-ref)))

;跳过所有空格
(define (nospace str cnt)
  (cond ((eq? (string-ref str cnt) #\space)
         (nospace str (+ cnt 1)))
        (else cnt)))

;格式化str
(define (format str)
  (define (iter cnt rem)
    (cond ((= (string-length str) cnt) rem)          
          ((string=? (to-string (string-ref str cnt)) "\n")
           (iter (+ cnt 1) rem))
          ((or (string=? (to-string (string-ref str cnt)) ",")
               (string=? (to-string (string-ref str cnt)) "|"))
           (iter (+ cnt 1) (string-append rem "\t")))
          ((eq? (string-ref str cnt) #\space)
           (iter (nospace str cnt) (string-append rem "\t")))          
          (else (iter (+ cnt 1) (string-append rem (to-string (string-ref str cnt)))))))
  (iter 0 ""))

;将格式化后的str转换为list
(define (to-list str)
  (define (iter cnt check)
    (cond ((= cnt (string-length str)) '())
          ((or (string=? (to-string (string-ref str cnt)) "\r")
               (string=? (to-string (string-ref str cnt)) "\t"))
           (cons check
                 (iter (+ cnt 1) "")))
          ((string=? (to-string (string-ref str cnt)) "\n")
           (iter (+ cnt 1) check))
          (else (iter (+ cnt 1)
                      (string-append check (to-string (string-ref str cnt)))))))
  (iter 0 ""))

;转换为最终列表，从这里开始分析
(define (ltt str)
  (define (iter ls tc cnt rem res)
    (cond ((null? ls) (append (reverse res) (list (reverse rem))))
          ((= tc cnt)
           (iter ls tc 0 '() (cons (reverse rem) res)))
          (else (iter (cdr ls) tc (+ cnt 1) (cons (car ls) rem) res))))
  (iter (to-list (format str)) 9 0 '() '()))

;生成可分析字符串列表 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define lst (ltt str))

;找出列表某一行中的某一列 ls = (list-ref lst line-id)
(define (find-item ls id)
  (cond ((= id 0) (car ls))
        (else (find-item (cdr ls) (- id 1)))))

;找出列表元素下标
(define (find-id ls item)
  (define (iter ls id)
    (cond ((null? ls) '())
          ((= (car ls) item) id)
          (else (iter (cdr ls) (+ id 1)))))
  (iter ls 1))

;*截取列表中的一段
(define (cut ls start end)
  (define (iter ls cnt rem)
    (cond ((null? ls) (reverse rem))
          ((and (>= cnt start)
                (< cnt end))
           (iter (cdr ls) (+ cnt 1) (cons (car ls) rem)))
          ((= end cnt) (reverse rem))
          (else (iter (cdr ls) (+ cnt 1) rem))))
  (iter ls 0 '()))

;将字符数列表转换为数字
(define (to-num ls)
  (map (λ (x) (string->number x)) ls))

;lls = 包含蓝球 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define lls (map (λ (x) (to-num (cut x 0 8))) lst))

;lss = 不包含蓝球 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define lss (map (λ (x) (to-num (cut x 0 7))) lst))

;test           ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ls16 (map (λ (x) (to-num (cut x 0 7))) (ltt s16)))

;item是否是ls的成员
(define (member? ls item cmp)
  (cond ((null? ls) #f)
        ((cmp (car ls) item) #t)
        (else (member? (cdr ls) item cmp))))

;item如果是ls的成员则返回ls
(define (member ls item cmp)
  (cond ((null? ls) '())
        ((cmp (car ls) item) ls)
        (else (member (cdr ls) item cmp))))

;分离出红球号码
(define red (map (λ (x) (to-num (cut x 1 7))) lst))

;分理出蓝球号码
(define blue (map (λ (x) (to-num (cut x 7 8))) lst))

;合并红篮球
(define ssq (map (λ (x y) (append x (cons y '()))) red blue))

;*统计某数item出现在lst中的频率 lst = red/blue ...
(define (freq lst item cmp)  
  (/ (exact->inexact (apply + (map (λ (x) (if (eq? x #t) 1 0)) (map (λ (x) (member? x item cmp)) lst))))
     (exact->inexact (length lst))))

;从序对中查找值 s = blue/red
(define (search ls num)
  (cond ((null? ls) '())
        ((= (cdar ls) num) (car ls))
        (else (search (cdr ls) num))))

;用第一个元素查找
(define (sear-fst lls x)
  (cond ((null? lls) '())
        ((= (caar lls) x) (car lls))
        (else (sear-fst (cdr lls) x))))

;从序对中删除x
(define (remove ls x)
  (cond ((null? ls) '())
        ((= (cdar ls) x) (cdr ls))
        (else (cons (car ls)
                    (remove (cdr ls) x)))))

;红球概率
(define freq-red (map (λ (x) (cons x (freq red x =))) (range 1 34)))

;蓝球概率
(define freq-blue (map (λ (x) (cons x (freq blue x =))) (range 1 17)))

;按概率排序 lsr = (sort (map (λ (x) (cdr x)) freq-red) >) lfr = freq-red
(define (sort-freq lsr lfr)
  (cond ((null? lsr) '())
        (else (cons (search lfr (car lsr))
                    (sort-freq (cdr lsr) (remove lfr (car lsr)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define sort-red (sort-freq (sort (map (λ (x) (cdr x)) freq-red) >) freq-red))
(define sort-blue (sort-freq (sort (map (λ (x) (cdr x)) freq-blue) >) freq-blue))

;计算#t出现的次数
(define (cnt-t ls)
  (define (iter ls cnt)
    (cond ((null? ls) cnt)
          ((eq? #t (car ls)) (iter (cdr ls) (+ cnt 1)))
          (else (iter (cdr ls) cnt))))
  (iter ls 0))

;*比对是否存在有相似列表 ls = 单列 lls = red/lss scale = 相似颗粒度
(define (same? ls lls scale)
  (filter (λ (x) (not (eq? '() x)))
   (map (λ (x y) (if (>= x scale) y '()))
       (map (λ (x) (cnt-t x))
            (map (λ (x)
                   (cons (member? x (find-item ls 0) =)
                         (cons (member? x (find-item ls 1) =)
                               (cons (member? x (find-item ls 2) =)
                                     (cons (member? x (find-item ls 3) =)
                                           (cons (member? x (find-item ls 4) =)
                                                 (cons (member? x (find-item ls 5) =)
                                                       '()))))))) lls)) lls)))

;ls为不定长列表 
(define (same ls lls)
  (define (iter lls len cnt rem res)
    (cond ((null? lls) (reverse res))
          ((= cnt len)
           (iter (cdr lls) len 0 '() (cons (reverse rem) res)))
          (else
           (iter lls len (+ cnt 1) (cons (member? (car lls) (find-item ls cnt) =) rem) res))))
  (iter lls (length ls) 0 '() '()))

;过滤出结果 rsame = (same ls lls)
(define (res-same rsame ls lls)
  (filter (λ (x) (not (eq? '() x)))
          (map (λ (x y) (if (= (cnt-t x) (length ls)) y '())) rsame lls)))

;*查找出含有ls的lss
(define (sear-same ls lss)
  (res-same (same ls lss) ls lss))

;计算某一期号码的概率：(map (λ (x) (assq x sort-red)) '(6 13 16 18 20 22))

;生成一组截止15年底的6*1899矩阵
(define matrix (make-list 1899 (make-list 6 0)))
(define mt33 (make-list 33 0))

;统计红球每个数出现的历史次数 lst = (range 1 34) / lst = (range 1 17) clr = blue / red
(define (history lst clr)
  (cond ((null? lst) '())
        (else (cons (cons (car lst)
                          (apply + (map (λ (x) (if (eq? x #t) 1 0)) (map (λ (x) (member? x (car lst) =)) clr))))
                    (history (cdr lst) clr)))))

(define his-red (history (range 1 34) red)) ;可以用(list-ref his-num id)来查看具体某个数出现的次数
(define his-blue (history (range 1 17) blue))

;联通概率 range = (remv num (range 1 34))
(define (con-freq num lss range)
  (define (iter range rem)
    (cond ((null? range) (reverse rem))
          (else (iter (cdr range)
                      (cons
                       (cons (car range)
                             (freq (filter (λ (x) (member? x num =)) lss) (car range) =))
                       rem)))))
  (iter range '()))

;计算出所有数字的联通概率
(define (connect rang)
  (cond ((null? rang) '())
        (else (cons (cons (car rang)
                          (con-freq (car rang) lss (remv (car rang) (range 1 34))))
                    (connect (cdr rang))))))

(define conn (connect (range 1 34))) ;可以通过(assq num conn)来取出某个数的联通概率

;按联通概率排序
(define (sort-conn rang)
  (cond ((null? rang) '())
        (else (cons
               (cons (car rang)
                     (sort-freq (sort (map (λ (x) (cdr x)) (cdr (assq (car rang) conn))) >) (cdr (assq (car rang) conn))))
               (sort-conn (cdr rang))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define sconn (sort-conn (range 1 34)))

;*将新数据插入至16年的文件中 line = '("2015154\t07,09,11,15,18,25,07\t2015-12-31")
;(write-to-file file-name "f:/ssq/res.txt" #:mode 'text)
(define (insert line)
  (display-lines-to-file line "f:/ssq/2016.txt" #:mode 'text #:exists 'append))

;*计算一列数据的概率链
(define (link-freq ls)  
  (cond ((null? (cdr ls)) (list (car ls)))
        (else (cons (car ls)
                    (cons (cdar (filter (λ (x) (= (cadr ls) (car x))) (cdr (assq (car ls) sconn))))
                          (link-freq (cdr ls)))))))

;*计算条件概率 ls = (link-freq ls)
(define (link ls)
  (define (iter ls)
    (cond ((= (last ls) (car ls)) (list (car ls)))
          (else (cons (car ls)
                      (cons (/ (cadr ls) (cdr (assq (car ls) freq-red)))
                            (iter (cddr ls)))))))
  (iter (link-freq ls)))

;*计算一列数字出现的概率 ls = (link ls)
(define (chance ls)
  (define (iter ls)
    (cond ((= (last ls) (car ls)) 1)
          (else (* (cadr ls) (iter (cddr ls))))))
  (iter (link ls)))

;计算一组数据的出现概率 (/ (apply + (map (λ (x) (chance x)) (map (λ (x) (cdr x)) ls16))) (length (map ...)))

;计算x,y...同时出现的概率，即联合概率分布 (exact->inexact (/ (length (sear-same '(1 11 21) lss)) (length lss)))

;从列表中删除列表或元素
(define (remove+ n ls)
  (cond ((list? n) (cdr (memv (last n) ls)))
        (else (cdr (memv n ls)))))

;生成遍历序列，无关顺序 ls = (range 1 34) len = 最长序列数
(define (generate ls len)
  (define (iter lr cnt rem)
    (cond ((= len cnt) rem)
          ((null? lr) (iter rem (+ cnt 1) rem))
          (else (iter (cdr lr) cnt (append rem (map (λ (x) (cons (car lr) (list x))) (remove+ (car lr) ls)))))))
  (iter ls 1 '()))

;生成最长为4的组合序列;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define comb (map (λ (x) (flatten x)) (generate (range 1 34) 4)))

;计算一组号码出现的概率，已将结果写入文本文件lind-comb
;(define linked (map (λ (x) (exact->inexact (/ (length (sear-same x lss)) (length lss)))) comb))
;(define link-comb (map (λ (x y) (cons x (list y))) comb linked))

;lc = link-comb
(define link-cb link-comb)

;查找lc表中数据并返回，同时将全局变量link-cb中查找到的内容删除
(define (sear-link x lc)
  (define (iter lc rem)
    (cond ((= x (cadar lc))
           (let ()
             (set! link-cb (append (reverse rem) (cdr lc)))
             (car lc)))
           (else (iter (cdr lc) (cons (car lc) rem)))))
  (iter lc '()))

;排序后的link-comb表，已写入文本文件
;(define sort-link-comb (map (λ (x) (sear-link x link-cb)) (sort linked >)))

;为sort-link-comb增添序号
(define sort-lc-id (map (λ (x y) (cons x y)) (range 1 52361) sort-link-comb))

;查找某个序列出现的全部情况 (sear-same '(11 21 23) (map (λ (x) (find-item x 1)) sort-lc-id))
;去掉sort-link-comb中的0 (define no-zero (filter (λ (x) (not (= (cadr x) 0))) sort-link-comb))
;检查两组列表是否相等
(define (list=? ls lt)
  (cond ((and (null? ls) (null? lt)) #t)
        ((or (null? ls) (null? lt)) #f)
        ((= (car ls) (car lt)) (list=? (cdr ls) (cdr lt)))
        (else #f)))

;在lss中查找ls lss = sort-link-comb
(define (find-list ls lss)
  (cond ((null? lss) #f)
        ((list=? ls (caar lss)) (car lss))
        (else (find-list ls (cdr lss)))))

;从sort-link-comb中过滤出长度为4的组合
(define comb-four (filter (λ (x) (if (= (length (car x)) 4) x #f)) sort-link-comb))

;ls = 过滤掉日期的纯粹6位数列表
(define (anlys ls)
  (map (λ (x) (find-list x comb-four)) (map (λ (x) (flatten x)) (generate ls 4))))


;过滤掉#f
(define (rem-f res)
    (cond ((null? res) '())
          (else (cons (filter (λ (x) (not (eq? #f x))) (car res))
                      (rem-f (cdr res))))))
