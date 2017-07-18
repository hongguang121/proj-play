#lang racket

(require pmap)
(require future-visualizer)
(require racket/flonum)

;并行强调一次计算中包含多个并列执行的任务
;并发强调函数允许多次非阻塞调用(近似于同时发生)

(define ! not)

(define a null)
(define b null)

(set! a 0)
(set! b 0)

;监听a,b的值，如果改变则重新调用sum
#|
(define listener
  (thread
   (λ ()
     (let loop ((ax a)
                (bx b))
       (if (or (! (= ax a)) (! (= bx b)))
           (begin         
             (displayln (format "a + b = ~a" (sum a b)))
             (loop a b))
           (begin
             (sleep 1)
             (loop ax bx)))))))
|#

(define (sum a b)
  (+ a b))

(define (fact n)
  (if (= n 0)
      1
      (* n (fact (- n 1)))))

(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1))
         (fib (- n 2)))))

;(set! a 1) ;res = 1

;(set a 1)
(define-syntax (set stx)
  (syntax-case stx ()
    [(_ key val)   
     #'(set! key val)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define env (make-hash '()))

;key: symbol
(define (event-on key val)
  (hash-set! env key val))

(define (event-emit key val callback)
  (let ((v (with-handlers ([exn:fail? (lambda (exn) (error "This key hasn't value"))])
             (hash-ref env key))))
    (unless (eq? v val)
      (event-on key val)
      (eval (list 'set! key val))
      (displayln (callback)))))

;1.(event-on 'a 1) 2.(event-emit 'a 3 (λ () (sum a b))) / (event-emit 'a 10 (λ () (fib a)))

;异步求值
(define (asyn fun callback)
  (thread
   (λ ()
     (let ((res (eval fun)))
       (callback res)))))

;(asyn '(fib 40) (λ (res) (displayln res)))
;(asyn '(fib 9) (λ (res) (asyn (fib res) (λ (res) (displayln res)))))

;消息通知机制即首先维护一个已有消息列表，当新消息进入后对比已有列表确定是否是新消息，如果是则通知，这么做降低了轮询开销

;loop-till
(define-syntax (loop-till stx)
  (syntax-case stx ()   
    [(_ condition body)    
     #'(let loop ()
         (when condition
           (body)
           (loop)))]))

(define i 5)
(loop-till (> i 0) (λ () (displayln i) (set! i (sub1 i))))

;filter upcase words
(filter (λ (x) (char-upper-case? x)) (string->list "Hello World"))

(define (mandelbrot iterations x y n)
  (let ([ci (fl- (fl/ (* 2.0 (->fl y)) (->fl n)) 1.0)]
        [cr (fl- (fl/ (* 2.0 (->fl x)) (->fl n)) 1.5)])
    (let loop ([i 0] [zr 0.0] [zi 0.0])
      (if (> i iterations)
          i
          (let ([zrq (fl* zr zr)]
                [ziq (fl* zi zi)])
            (cond
              [(fl> (fl+ zrq ziq) 4.0) i]
              [else (loop (add1 i)
                          (fl+ (fl- zrq ziq) cr)
                          (fl+ (fl* 2.0 (fl* zr zi)) ci))]))))))

#|
(define pls (pmapp-c-start 4))
(define f '(lambda (iterations x y n) (mandelbrot iterations x y n)))
(pmapp-c pls f '(10000000) '(62) '(500) '(1000))
(pmapp-c-stop pls)
|#

;(define res (time (pmapf + (range 1 100000) (range 1 100000))))
;(define res (time (pmapp '+ (range 1 100000) (range 1 100000))))
;(define pls (pmapp-c-start 4))
;(define res (time (pmapp-c pls '+ (range 1 100000) (range 1 100000))))
;(pmapp-c-stop pls)