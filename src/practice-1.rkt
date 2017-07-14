#lang racket

(require future-visualizer)
(require racket/flonum)
(require pict/tree-layout)
(require "new-sort.rkt")


;尾递归主要是指递归调用中不产生类似于(* n (fact (- n 1)))中*n的操作。
;不要忘记eval可以求值'(list ...)

#|并发并不意味着性能的提升，而是说普通阻塞单线程的计算模式在一定时间内只能处理一个函数，而并发意味着
  一定时间内可以处理多个相同函数或不同函数，并且不产生阻塞，但是并发并不是并行，这里只是非阻塞而已，
  并没有真正带来性能的提升。|#

#|关于多线程和异步的差异，在js计时函数setTimeout的使用中，回调是一个用户定义函数，在多线程计时函数
  set-time-out中，fn是一个用户定义函数；多线程执行方式是将fn直接交给一个线程，不阻塞主线程并执行，
  产生结果后直接返回；而异步会在执行setTimeout时将其交给一个线程（事件队列），该线程先执行计时器，同时，
  主线程并不阻塞，而是继续执行接下来的函数，当线程执行计时器结束后，则将用户函数排入队列等待执行，
  如果主线程此时正在忙碌，则会等待空闲时执行,为何不在计时结束后直接执行用户函数呢？这样和多线程就会
  相同了，可能是考虑到直接执行会产生一些特殊而复杂的情形，比如变量在主线程中正在改变中，那么等主线程
  修改变量结束后才执行回调会比较安全；注意回调在该层次上才具有真正的意义|#

#|关于性能优化：1.将相同代码段合并成函数2.不要像斐波那契数列递归计算那样产生过多的重复性计算3.如果要实现
  的某个功能已存在系统提供的实现函数，则调用系统提供的函数4.活用map，apply，filter等高阶函数5.使用缓存
  6.减少I/O操作，在内存中完成数据存取7.适当地使用线程，进程进行并发，异步，并行，惰性计算8.编写模块化函数，
  函数的功能要专一，代码简洁，解耦合，可以作为复用部件添加到其他函数中，类似于系统提供的库函数9.将数据运行
  在适当的数据结构上10.好的设计和性能优化测试|#

#|并发更像是一种能力，一个有并发能力的程序可以被并行执行，也可以作为单线程执行；而并行是实际执行的的方式，
  当程序是并行执行的，则意味着一定是运行在多个线程，进程，CPU上的
  例如，fib函数不是并发的，但可以并行计算，但它本身不具备并发的能力，而一个并发版本的fib则意味着它
  在同一时刻可以同时创建多个可执行副本|#


;尾递归：如果函数在最后调用了自己，通常递归会渐渐积累起一条递归栈，空间消耗会越来越大，而尾递归
;       就像是fold函数，它会在调用处进行求值规约，于是递归调用被替代为一个简单的跳转，从而大大
;       节省了空间消耗
;有状态和无状态：根据有无因时间而改变的变量存在与否区分，如果有就是有状态，否则为无状态

;逻辑需要简单直白直接，这样才能有效减少错误，或者高效排查错误，变量的作用域至关重要，必须清晰，每个变量名指代的含义越少越好，
;名字越清楚越好，即便很长也没有关系，指向越明确越好，变量的生存空间一定要清晰，使用完的变量应当及时销毁；最大程度
;地减少交错耦合;

;闭包是说函数定义的同时绑定在定义时的变量环境，从而形成一个封闭自足不受外界环境变量干扰的系统

;在赋值时，一定要注意全局变量因为和局部变量同名而被覆盖的情形

#|这是
多段注释|#

#i5 ;转换成非精确小数，位数有限
#e5 ;转换为精确表达

(define (guess)
    (let ((rd (random 10))
          (gs 0))
      (let loop ()       
        (display "Please guess a number(0-9): ")
        (set! gs (read))
        (if (number? gs)
            (cond ((= gs rd) "Correct!")
                  ((< gs rd) (begin (print "More Bigger! Try again: ") (loop)))
                  ((> gs rd) (begin (print "More Smaller! Try again: ") (loop))))
            (print "Please enter a number.")))))

(define (make-posn x y)
  (cons x y))

(define (posn-x mp) (car mp))
(define (posn-y mp) (cdr mp))

(define (distance-to-0 mp)
  (sqrt (+ (* (posn-x mp) (posn-x mp))
           (* (posn-y mp) (posn-y mp)))))

(define-struct people (name sex age))
(define p1 (make-people "Yao" "female" 17))
(define p2 (make-people "Li" "female" 21))
(define p3 (make-people "Wang" "male" 23))

(people-name p1)
;(map (λ (x) (people-name x)) `(,p1 ,p2 ,p3))

;check email address
(regexp-match #px"^[\\w]+@[\\w]+[\\.][\\w]+$" "abc@169.com")
(regexp-match #px"^[A-Z][\\w]+ing" "Introducing current")

(define (comp arg1 arg2)
    (cond ((> arg1 arg2) 1)
          ((< arg1 arg2) -1)
          (else 0)))

(let ((x 1))
    (define add
      (λ (y) (+ x y)))
    (print (add 1))
    (let ((x 2))
      (add 4)))

(define echo
    (λ args
      (let ((ls args));((ls (car args)))此种情况针对参数本身是一个列表的情况
        (let loop ((ls ls))
          (if (null? ls)
              (print "end")
              (begin
                (print (car ls))
                (loop (cdr ls))))))))

;; Finds Racket sources in all subdirs
(for ([path (in-directory)]
      #:when (regexp-match? #rx"[.]rkt$" path))
  (printf "source file: ~a\n" path))

(define fact
    (thread (λ ()
              (let ((n (thread-receive)))
                (define (iter n res)
                  (if (= n 0)
                      (printf "~a~n" res)
                      (iter (- n 1) (* n res))))
                (iter n 1)))))

(thread-send fact 6)

(define (fibonacci n)
  (if (< n 2)
      n
      (+ (fibonacci (- n 1))
         (fibonacci (- n 2)))))

;(thread-receive)只能接收一次
(define kons
  (thread (λ ()
            (let* ((ls (thread-receive))
                   (x (car ls))
                   (y (cadr ls)))
              (printf "~a ~a~n" x y)))))

(thread-send kons '(1 2))

(define stop (thread (λ () (let ((s (thread-receive))) (printf "~a~n" "休眠3秒")))))

;(thread-send stop (sleep 3))

;(lst-mani '(1 2 3 4 5))
(define (lst-mani ls)
    (let* ((len (length ls))
           (left (take ls (floor (/ len 2))))           
           (right (drop ls (floor (/ len 2)))))
      (define (iter left right)
        (cond ((null? left) right)
              (else (cons (car left)
                          (cons (car right)
                                (iter (cdr left) (cdr right)))))))
      (iter left right)))

(define (to-string str-ref)
  (list->string (list str-ref)))
;匹配括号，必须数量和位置一致方为真
(define (match-bracket str)
  (let ((len (string-length str))
        (string (λ (cnt) (to-string (string-ref str cnt)))))
    (define (revert ls res)
      (cond ((null? ls) res)
            ((string=? (car ls) "{") (revert (cdr ls) (cons "}" res)))
            ((string=? (car ls) "[") (revert (cdr ls) (cons "]" res)))
            ((string=? (car ls) "(") (revert (cdr ls) (cons ")" res)))))
    (define (check ls)
      (let ((left (revert (caar ls) '()))
            (right (cadr ls)))
        (define (iter ll lr)
          (cond ((null? ll) (null? lr))
                ((string=? (car ll) (car lr)) (iter (cdr ll) (cdr lr)))
                (else #f)))
        (iter left right)))
    (define (iter cnt left right)
      (cond ((= cnt len) (check (cons (list left) (list right))))
            ((or (string=? (string cnt) "{")
                 (string=? (string cnt) "[")
                 (string=? (string cnt) "("))
             (iter (add1 cnt) (cons (string cnt) left) right))
            ((or (string=? (string cnt) "}")
                 (string=? (string cnt) "]")
                 (string=? (string cnt) ")"))
             (iter (+ cnt 1) left (cons (string cnt) right)))
            (else (iter (add1 cnt) left right))))
    (iter 0 '() '())))

;文法作用域测试
(let ((x 1))
  (define fun (λ (y) (* x y)))
  (let ((x 2))
    (fun 2)))

;((λ (a b) (+ a b)) 1 2)
;(((λ (y) (λ (x) (* x y))) 3) 4)

(define f 'fun)
(set! f (let ((x 1))
          (λ (y) (* x y))))
(let ((x 2))
  (f 4))

;闭包
#|
(define (count)
  (let loop ((i 0))
    (if (> i 3)
        '()
        (cons (λ () (* i i))
              (loop (add1 i))))))
|#

((let ((i 1))
    (λ () (* i i))))

#|
(for ((c (count)))
  (displayln (c)))
|#

(let ((x 1))
  (let ((y 2))
    (let ((f (λ (x y) (+ x y))))
      (let ((x 3))
        (f x y)))))

(let ((x 3))
  (define  (add y) (+ x y))
  (let ((x 5)
        (y 4))
    (add y)))

;prime
(define (prime? n)
  (if (= n 2) #t
      (let ((end (ceiling (sqrt n))))
        (let loop ((sta 2))
          (if (= (remainder n sta) 0)
              #f
              (if (= sta end)
                  #t
                  (loop (add1 sta))))))))
        
;回调    
(define (foreach ls callback)
  (let loop ((lt ls)
             (cb callback)
             (res '()))
    (cond ((null? lt) (reverse res))
          (else (loop (cdr lt) callback (cons (apply callback (list (car lt))) res))))))

(foreach (range 0 10) (λ (x) (add1 x)))

;定时器，并且可以通过bk参数进行中断
(define (break fn)
  (set! fn "break"))

(define result '())
(define c-res '())
;异步执行其他过程，在result返回后将结果输出
(define (sh)
  (let ()    
    (let loop ()
      (if (null? c-res)
          (begin
           (writeln "Do something...")
           (loop))
          c-res))))

;创建线程执行函数
(define (th fn)
  (thread (λ ()
            (let ((res fn))
              (set! c-res res)))))

(define (set-time-out fn time . bk)    
  (if (and (not (null? bk))(eq? (car bk) #t))
      (break fn)
      (begin
        (th fn)
        (sleep time)        
        (sh))))

;多线程计算(fibonacci 30)
;(apply + (list (set-time-out (fibonacci 29) 0) (set-time-out (fibonacci 28) 0)))

;结果通知线程，一直在后台监视result变量的变化
(define sirase
  (thread (λ ()            
            (let loop ();((res (thread-receive)))
              (if (null? result)
                  (begin
                    (sleep 5)
                    (loop))
                  (begin
                    (printf "~a~\n" result)
                    (newline)
                    (set! result '())
                    (loop)))))))


;必须通过(send)函数启动或关闭
(define (set-iter name)
  (thread (λ ()          
            (let ((ls (thread-receive))
                  (th (current-thread)))
                  ;(stop (λ (k) (set! result k))))
              (if (= (length ls) 3)
                  (let* ((fn (first ls))
                         (pm (delay/thread `,fn))
                         (time (second ls)))
                    (let loop ((id 0))                      
                      (set! c-res (cons id (cons name (cons th (list (force pm))))))
                      (sleep time)
                      (loop (add1 id))))
                  (error "参数错误"))))))

;(procedure-arity cons) 返回函数参数个数

;多线程计算，第一个参数是函数名，之后是函数的参数
;(thread-send (multi-thread) `(,fibonacci 30))
;(multi-thread fibonacci 30)
(define output-semaphore (make-semaphore 1))

(define (multi-thread . para)
  (thread (λ ()
            (let* ((th (current-thread))                   
                   (para (if (null? para)
                             (thread-receive)
                             para))
                   (start (mytime)))
              (call-with-semaphore
               output-semaphore
               (λ ()
                 (set! result                    
                       (append `(,(first para) args: ,@(rest para) result:)
                               (cons (apply (first para) (rest para)) 
                                     (list 'time: (- (mytime) start) 'ms))
                               result))
                ))))
))
              ;(thread-send sirase res)))))

;用for,list分发线程和复写分发会有计算时间上的差距，目前还不清楚原因
;(for ((i 5))
    ;(thread-send (multi-thread) `(,fibonacci 30)))
;    (multi-thread fibonacci 30))

;启动线程(send (set-iter "fib 30") (fibonacci 30) 3 #f)
;终止线程(send (set-iter "fib 30") (fibonacci 30) 3 #t)
(define (send fun . para)  
  (if (last para)
      (begin 
        (thread-suspend (third c-res))
        c-res)
      (thread-send fun para)))

(define test
  (thread (λ ()
            (let loop ((bk (thread-receive))
                       (th (current-thread))
                       (cnt 0))                       
              (set! result (cons th (list cnt)))
              (loop bk th (add1 cnt))))))

;synchronized
;多个线程交替修改同一变量是没有意义的，这等同于一个单线程程序，但也并非完全如此，有时候不同的线程为了
;相互通信，可以通过一个共享变量来完成
;一个共享变量i
(define i '(0))

;也可以通过(thread-wait)
;注意：在racket中变量是atom，但函数不是atom，必要的时候需要同步
(define (iter-syn . len)
    (thread (λ ()
              (let ((len (thread-receive)))                
                (call-with-semaphore
                 output-semaphore
                 (λ ()
                   ;(for ((i (car len)))
                     ;(displayln i))))))))
                   ;(displayln (apply + (range (car len))))
                   ;(for ((n (car len)))                     
                     ;(set! i (cons (add1 (car i)) i)))
                   (for ((n 5))
                     (displayln n))
                   ))))))

;创建多线程修改i值
;(for ((i 3))
    ;(thread-send (iter-syn) '(10)))

;非同步版，多个线程会交错显示n值，而不是一个线程显示完毕后下一个线程再继续显示
(define (iter . len)
    (thread (λ ()
              (let ((len (thread-receive)))                              
                (for ((n 5))
                  (displayln n))))))

(define s 0) ;atom 因为s是原子变量，它不可以在修改的过程中被其他线程同时修改，所以是线程安全的

(define (sum)
  (thread
   (λ ()
     (for ((i 1000))
       (set! s (add1 s))))))

;(for ((i 2))
;    (thread sum))

#|In contrast to thread, which provides concurrency（并发） for arbitrary computations
without parallelism（并行）, future provides parallelism for limited computations.|#

(let ([f (future (lambda () (+ 1 2)))])
    (list (+ 3 4) (touch f)))

;Y-combinator
(define Y
  (λ (le)
    ((λ (f) (f f))
     (λ (f)
       (le (λ (x)
             ((f f) x)))))))

((Y (λ (fib)
      (λ (n)
        (if (< n 2) n
        (+ (fib (- n 1))
           (fib (- n 2))))))) 12)

((Y (λ (fact)
      (λ (n)
        (if (= n 0) 1
            (* n (fact (- n 1))))))) 7)

;主线程
(define main-thread (current-thread))
;事件队列
(define event-ls '(("Event-name" "Event-body")))
;事件结果缓存，保证相同事件只调用一次
(define cache '(("Event-name" "Event-content" "result")))
;订阅事件 fname = symbol
;事件处理方式相较于动态检查传入函数参数再匹配相应的处理方式而言更加准确，方便，在事件订阅阶段一切都
;已经准备就绪了。
(define (emitter-on fname fn)
  (set! event-ls (cons (list fname fn) event-ls)))
;发布
(define (emitter-emit thread-id)
  (thread
   (λ ()
     (let loop ()
     (define f (channel-get work-channel))
     (let ((fname (first f))
           (content (second f)))
  (let ((evt (assq fname event-ls))
        (result (assq fname cache)))
    (if (eq? #f (not evt))
        (if (and (eq? #f (not result)) (eq? content (cadr result)))
            (channel-put result-channel (format "~a = ~a" (list fname content) (last result))) ;从缓存中查找计算结果
            (let ((res (apply (cadr evt) (list content))))
              (set! cache (cons (list fname content res) cache))
              (channel-put result-channel (format "~a = ~a" (list fname content) res))))
        (error "No event!"))))
       (loop)))))

(emitter-on 'square (λ (x) (* x x)))
(emitter-on 'fact
              (λ (n)
                (let loop ((i n)
                           (res 1))
                  (if (= i 0)
                      res
                      (loop (- i 1) (* i res))))))
(emitter-on 'fib
              (λ (n)
                (let loop ((i 0)
                           (a 0)
                           (b 1))
                  (if (= i n)                      
                      a
                      (loop (+ i 1) b (+ a b))))))

(emitter-on 'fib-rec
            (Y (λ (fib)
                 (λ (n)
                   (if (< n 2) n
                       (+ (fib (- n 1))
                          (fib (- n 2))))))))

(define q (list '(fact 6)
                '(square 9)
                '(fib 10)))

(define result-channel (make-channel))
(define result-thread
  (thread
   (λ ()
     (let loop ()
       (displayln (channel-get result-channel))
       (loop)))))

(define work-channel (make-channel))

(define work-threads (map emitter-emit '(1 2)))

;map并不消费列表，而for消费列表
(for ((e q))
  (channel-put work-channel e))

;(for-each thread-wait work-threads)

;promise
(define prime (delay (prime? 9))) ;在force时才会被求值
(promise? prime) ;#t
(promise-forced? prime) ;#f
(force prime) ;此时被求值，同时将该值缓存，以后每次求值时都返回缓存中的结果
(promise-running? prime) ;#f

;多参数函数
(define (args . arr)
    (apply + arr))

;并行计算
(define (any-double? l)
  (for/or ([i (in-list l)])
    (for/or ([i2 (in-list l)])
      (= i2 (* 2 i)))))

(define l1 (for/list ([i (in-range 5000)])
             (+ (* 2 i) 1)))
(define l2 (for/list ([i (in-range 5000)])
             (- (* 2 i) 1)))

;***
(define (computation-1)  
  (let ((f (future (λ () any-double? l2))))
    (or (any-double? l1)
        ;(any-double? l2) ;用(how-long computation-1)来进行性能测试
        (touch f) ;比上式减少一半时间
        )))

;一个斐波那契的例子
(define (cp n)
    (let ((fib-left (future (λ () (fibonacci (- n 1)))))
          (fib-right (future (λ () (fibonacci (- n 2))))))
      (+ (touch fib-left)
         (touch fib-right))))

;(time (fibonacci 40)) cpu time: 18625 real time: 18675 gc time: 497
;(time (cp 40))        cpu time: 15609 real time: 10126 gc time: 47

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

(define (computation-2)
  (visualize-futures
  (let ([f (future (lambda () (mandelbrot 10000000 62 501 1000)))])
    (list (mandelbrot 10000000 62 500 1000)
          ;(mandelbrot 10000000 62 501 1000)
          (touch f)
          ))))

;(apply sqr (list (apply * (list 2 (apply + (list 0 (add1 1)))))))
;(super-apply sqr * 2 + 1 1)
;特别注意：#<procedure:+>只可以通过apply使用，所以apply不可以是#<procedure:apply>。
;list，cons会将过程求值成#<procedure:>的形式。
;函数参数args是一个list，所以过程在list中已被计算，只能通过apply使用，而非解引用计算。
(define (super-apply . args)  
  (let loop ((ls (reverse args))
             (res '()))
    (cond ((null? ls) (car res))
          ((procedure? (car ls))
           ;(loop (cdr ls) (append (list apply (car ls)) (list res))))
           (loop (cdr ls) (list (apply (car ls) res))))
          (else (loop (cdr ls) (cons (car ls) res))))))

;case参数必须是一个过程
(case ((λ () 'get))
    ((get) 'get)
    ((post) 'post))

;循环性能测试 大约 1200W/s,二进制格式大约5600W/s
(define (test-iter)
  (let loop ((start (mytime))
             (id 0))
    (if (> (- (mytime) start) 1000)
        id
        (loop start (add1 id)))))

;信号量
(define count 0)
(define (increment name)
    (thread (λ ()
              (let ((th (thread-receive)))
                (call-with-semaphore ;通过信号量实现同步,否则会出现线程的互相干扰
                 output-semaphore
                 (λ ()
                   (for ((i 10))
                     (set! count (add1 count))
                     (printf "~a: ~a\n" name count))))))
            ))

(define (dispatch-th)
  (for ((t '("th1" "th2" "th3")))
    (thread-send (increment t) '())))

;因式分解
(define (factor num)
  (if (prime? num)
      "It's prime"
      (let loop ((num num)
                 (factor 2)
                 (res '()))
        (cond ((prime? num) (reverse (cons num res)))
              ((zero? (remainder num factor))
               (loop (/ num factor) 2 (cons factor res)))
              (else (loop num (add1 factor) res))))))

;关于进程
;(system "command") 同步处理命令行
;(process "command") 异步处理命令行

;异常处理是一种一对多的模式匹配，在众多可能的处理方式中匹配到相应的处理方式。

;测试
(define (assert fun value message)
  (if (= fun value)
      #t
      message))

;批量测试
;(batch (list (fibonacci 10) (fibonacci 0) (fibonacci 1)) '(55 2 1) '("false" "false" "false"))
(define (batch lf lv lm)
  (map (λ (f v m) (assert f v m)) lf lv lm))

;callback
;回调函数cb用于获取线程返回值
;(fib 10 (λ (x) (displayln x)))
;(thread-send fib `(10  ,(λ (x) (displayln x))))
(define (fib)
  (thread (λ ()
            (let ((args (thread-receive)))
              (let ((n (first args))
                    (cb (second args)))
                (let loop ((cnt n)
                           (a 0)
                           (b 1))
                  (if (= cnt 0)
                      (cb a)
                      (loop (sub1 cnt) b (+ a b)))))))))

;(for ((i 10))
;    (thread-send (fib) `(,i ,(λ (x) (displayln x)))))
;(for ((i 10))
;    (thread-send (fib) `(,i ,(λ (x) (set! result (cons (cons i x) result))))))

;actor
(define talker
  (thread
   (λ ()
     (define count 0) ;有状态计数器
     (let loop () ;使线程一直存活下去
       (define rec (thread-receive))
       (when (and (eq? 'iter (car rec))
                  (not (null? (cdr rec))))
         (set! count (second rec))) ;如果iter有参数则设置
       (match (first rec)
         ('greet (printf "Hello ~a\n" (second rec)) (loop))
         ('praise (printf "~a, you're amazing\n" (second rec)) (loop))
         ('iter (printf "count: ~a\n" (add1 count)) (set! count (add1 count)) (loop))
         ('done (displayln "done"))))))) ;终止该线程

(define talk '((greet World) (praise Yaoer) (greet hg)))
(for ((e talk))
  (thread-send talker e))


;因为没有线程池，无法保证线程顺序
(define records '()) ;避免因线程不同而被清楚
(define (commune name . history?) ;不同于talker的是该线程不是thunk
  (thread
   (λ ()     
     (if (and (not (null? history?))
              (eq? #t (car history?)))
         (displayln (filter (λ (x) (eq? (car x) name)) records))
         (call-with-semaphore  ;保证操作该函数的线程独占处理过程，处理内容原子化
          output-semaphore
          (λ ()
            (define reply (thread-receive))       
            (printf "from ~a: ~a\n" name reply)
            (set! records (cons (list name reply) records))))))))

(thread-send (commune 'Yaoer) "Hello! Anybody here?")
(thread-send (commune 'Hg) "Hi, I'm here")
(thread-send (commune 'Yaoer) "How are you?")
(thread-send (commune 'Hg) "I'm fine")
;(thread-send (commune 'Yaoer #t) "")

(define res '())

(define pmap-channel (make-channel))

(define (pmap thread-id)
  (thread
   (λ ()
     (call-with-semaphore ;保证顺序
      output-semaphore
      (λ ()
        (let loop () ;关键点
          (define e (channel-get pmap-channel))
          (case e
            ((done) res)
            (else          
             (set! res (cons (map add1 (list e)) res))
             (loop)))))))))

(define pmap-threads (map pmap '(1 2))) ;用两个线程交替计算

(define (compute lst)
      (for ((e lst))
        (channel-put pmap-channel e)))

(compute (append (range 10) '(done done))) ;通过done来终止两个线程

(for-each thread-wait pmap-threads)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define redbag 10)

(define (grab)
  (thread (λ ()
            (let ((user (thread-receive)))
              (if (> redbag 0)
                  (begin
                    (set! redbag (sub1 redbag))
                    (displayln (format "user ~a get 1 redbag" user)))
                  (displayln (format "no redbag have")))))))

(for ((p 4))
    (for ((i 3))
      (thread-send (grab) (number->string i))))