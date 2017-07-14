#lang racket

;内联(inline)也可以节省开销，把完成同一件事情的函数组合在一起形成内联，从而只有在解决该事时这些函数才发挥作用。
;本次主要练习作用域相关内容以及行为参数化
;程序的主要bug之一是变量名称可能已脱离了作用域却还在使用，另一方面是对引用和值以及有效作用域的范围
;产生了误判，还有逻辑错误，所以在写代码前就应该将问题的描述考虑清楚
;行为参数化，首先应该考虑apply，之后是eval，macro也应考虑

;遍历器

(define rem '())

;ls = 'ls exp:(iterator 'ls)
(define (iterator ls)
  (let ()
    (set! rem (cons `(,ls ,(eval ls)) rem))))

;name = symbol exp:(next 'ls)
(define (next name)
  (let ((curr-obj (assoc name rem)))
  (if curr-obj
      (if (null? (cadr curr-obj))
          '()
          (let ((res (caadr curr-obj)))
            (set! rem (remove* (list name) rem (λ (x y) (eq? x (car y)))))
            ;(printf "~a\n" (caadr curr-obj))
            (set! rem (cons (cons (car curr-obj)
                                  (cons (cdadr curr-obj) '()))
                            rem))
            res))
      "Wrong name!")))

(define ls '(a b c d e f g))
(define lt '(1 2 3 4 5 6 7))

;(eval (string->symbol "ls"))

;自动生成随机数字，并输出到文件 n = 输出数字个数 r = 小于r的随机数
(define (rand-num n r)
  (letrec ((R (lambda (cnt)
                (if (= cnt n)
                    '()
                    (cons (random r)
                          (R (+ cnt 1)))))))
    (R 0)))

;debug
(define cont '())

(define (fact-debug n)
  (if (= n 0)
      "(fact 0) = 1)"
      (string-append "(* " (number->string n) " " ;" (fact " (number->string (sub1 n)) ")) "
                     (fact-debug (- n 1)))))

(define (fib-debug n)
  (if (< n 2)
      (string-append "(fib " (number->string n) ") = " (number->string n) ")")
      (string-append ;"(+ " "(fib " (number->string (- n 1)) ") "
                     ;      "(fib " (number->string (- n 2)) ")) "
                     "(" (fib-debug (- n 1)) " + " 
                     "(" (fib-debug (- n 2)))))

(define (trace s v flag)
  (unless (not flag)
    (printf s v)))

(define (debug val)
  (call/cc (λ (k) (k (printf "~a\n" val)))))

(define (fact n . act)
  ;(let ((@printf (trace "fact(~a)\n" n #t)))
  (let ((insert act))
    (let loop ((n n))
  (if (= n 0)
      1
      (let ()
        ;@printf
        ;(debug n)
        (if (null? insert)        
            (* n (loop (- n 1)))
            (begin
              (eval `(,(car insert) ,n));n在该函数作用域内，可以解引用))))))
              (* n (loop (- n 1))))))))))

;自定义排序
(define lsn '(2013UK0301 2011US0125 2014UN0256))

(sort (map symbol->string lsn) #:key (λ (x) (substring x 4 6)) string>=?)

;进行操作系统层面的操作(system command) command:string
;(system "raco")

;eval接受symbol型参数使参数行为化

;定义@check标记，并通过@insert为其插入函数，之后可以将@check嵌入到需要插入函数的函数中，
;然后将变量传递个@check进行求值
(define @check (λ (x) x))

;(@insert '(fact+ 6) '(λ (x) (printf "Current n is ~a\n" x)))
;fun/act : symbol
(define (@insert fun . act)
  (let ((ls (car act)))
    (unless (null? ls)
      ;(for ((a ls))
      ;  (eval a))
      (set! @check (eval ls))
      (eval fun))))

;(fact 6 '(λ (x) (displayln x)))

;(@insert '(λ (x) (displayln x)))
(define/match (fact+ n)
  ([0] 1)
  ([n] (begin (@check n) (* n (fact+ (- n 1))))))

;delay可以对该作用域内尚未求值的变量的行为进行定义，在求值后通过force来使其产生新的行为
;原本想要通过(eval expr)嵌入到需要debug的函数中来检查该函数变量的变化，但是该函数的变量
;在定义expr的时候并不存在，而且他们也不在同一个作用域内，所以无法通过此方法debug，只能将
;表达式的检测函数写下，传递给该函数，在该函数体内向表达式的检测函数传递需要检测的值

;delay可以用来进行异步求值
(define (test-delay fun . args)
    (let* ((cont "test")
           (expr (delay ((λ (c) c) cont))))
      (set! cont (apply fun args))
      (force expr)))

(define stx #'(fact 6))
(syntax->list stx)

;在排序过程中消除重复值
(define (qsort ls)  
  (if (< (length ls) 2)
      ls
      (let ((flag (car ls)))
        (append (qsort (filter (λ (x) (< x flag)) ls))
                (list flag)
                (qsort (filter (λ (x) (> x flag)) ls))))))

;delay
(let ((d (delay (future (λ () (λ (a b) (+ a b)))))))
    (let ((a 1)
          (b 2))
      ((touch (force d)) a b)))

(define result '())

(define (promise)
  (thread
   (λ ()
     (let loop ((receive (thread-receive)))                
       (if (null? receive)
           (loop (receive (thread-receive)) )
           (set! result receive))))))

(define mean-of-life (promise))
(define p (λ () (display "The meaning of life is ") result))

;(deliver mean-of-life 'find)
(define (deliver to val)
  (thread
   (λ ()
     (thread-send to val))))

;这种依赖于顺序的并行流的效率并不高，但在并行计算斐波那契等数列时，效率会提升
(define la (range 500000))
(define lb (range 500000 1000000))
(define lc (range 1000000 1500000))
(define ld (range 1500000 2000000))
(define (parallel-sum)
  (let ((sa (future (λ () (apply + la))))
        (sb (future (λ () (apply + lb))))
        (sc (future (λ () (apply + lc))))
        (sd (future (λ () (apply + ld)))))
    (+ (touch sa)
       (touch sb)
       (touch sc)
       (touch sd))))

;(time (parallel-sum))
;(time (+ (apply + la)(apply + lb)(apply + lc)(apply + ld)))

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

(define res '())

(define output-semaphore (make-semaphore 1))

;需要同步写入res，否则res会被线程篡改
(define (send . ls)
  (thread
   (λ ()
     (call-with-semaphore
      output-semaphore
      (λ ()
        (set! res (cons (merge-sort (car ls)) res)))))))

(define (listener part)
  (thread
   (λ ()
     (let loop ((r res))
       (if (or (not (= (length res) part)) (null? res))
           (loop res)
           (let ()
             (sirase res)
             (loop res)))))))

(define (sirase r)
  (set! res (foldl (λ (a r) (merge r a)) '() r)))

;ls：待排序列表 part：分解为n部分
(define (assign ls part)
  (let ((cut (floor (/ (length ls) part))))
    (let loop ((cnt 0)               
               (ls ls))
      (if (= cnt part)
          (listener part)
          (let ()
            (send (take ls cut))
            (loop (add1 cnt)                
                  (drop ls cut)))))))

;(assign '(8 6 4 2 7 5 3 1) 2)

;(list (send '(7 5 3 1)) (send '(8 6 4 2)))
;(merge (first res) (second res))

;合并多个列表计算结果
;(foldl (λ (a res) (merge res a)) '() (list (first res) (second res)))

(define console '())

(set! console 'hi)

;监听线程，但是效率低下
#|
(define echo
  (thread
   (λ ()
     (let loop ()
       (if (null? console)
           (loop)
           (let ()
             (displayln console)
             (set! console '())
             (loop)))))))
|#

(define (lazy-sum n)  
  (delay (apply + (range n))))