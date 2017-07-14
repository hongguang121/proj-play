#lang racket

;本次练习主要集中在并发


#|
(define res '())

(define (iter n)
  (thread
   (λ ()
     ;(let ((rem '()))
     (for ((i n))
       (displayln (format "thread: ~a" i))       
       ))))

(define (main-iter n)
  (for ((i n))
    (displayln (format "main-thread: ~a" i))))

(for ((i 3))
  (let ()
    ;(iter 5)
    (thread-wait (iter 5))
    (main-iter 3)))

(define worker (thread (lambda ()
                         (let loop ()
                           (displayln "Working...")
                           (sleep 0.2)
                           (loop)))))
(sleep 2.5) ;主线程暂停2.5s
(kill-thread worker)

(define worker+ (thread
                 (lambda ()
                   (for ([i 25])
                     (printf "Working hard... ~a~n" i)))))
(thread-wait worker+)
(displayln "Worker finished")


(define worker-thread (thread
                       (lambda ()
                         (let loop ()
                           (match (thread-receive)
                             [(? number? num)
                              (printf "Processing ~a~n" num)
                              (loop)]
                             ['done
                              (printf "Done~n")])))))
(for ([i 20])
  (thread-send worker-thread i))
(thread-send worker-thread 'done)
(thread-wait worker-thread)

|#

(define (make-arithmetic-thread operation)
  (thread (lambda ()
            (let loop ()
              (match (thread-receive)
                [(list oper1 oper2 result-thread)
                 (thread-send result-thread
                              (format "~a ~a ~a = ~a"
                                      oper1
                                      operation
                                      oper2
                                      (operation oper1 oper2)))
                 (loop)])))))
 
(define addition-thread (make-arithmetic-thread +))
(define subtraction-thread (make-arithmetic-thread -))
 
(define worklist '((+ 1 1) (+ 2 2) (- 3 2) (- 4 1)))
(for ([item worklist])
  (match item
    [(list '+ o1 o2)
     (thread-send addition-thread
                  (list o1 o2 (current-thread)))]
    [(list '- o1 o2)
     (thread-send subtraction-thread
                  (list o1 o2 (current-thread)))]))
 
(for ([i (length worklist)])
  (displayln (thread-receive)))

#|Semaphores facilitate synchronized access to an arbitrary shared resource.
  Use semaphores when multiple threads must perform non-atomic operations on a single resource.
  当多个线程共享单一非原子资源时应当使用信标来同步共享资源
  By using a semaphore initialized with a count of 1, only one thread will print at a time.
  信标的计数值意味着同一时间允许多少线程来访问共享资源，所以一般设置为1
|#

(define output-semaphore (make-semaphore 1))

(define (make-thread name)
  (thread (lambda ()
            (for [(i 3)] ;信标在for循环内
              (call-with-semaphore
               output-semaphore
               (lambda ()
                (printf "thread ~a: ~a~n" name i)))))))

(define threads
  (map make-thread '(A B C)))

(for-each thread-wait threads)

;#|

(define result-channel (make-channel))
(define result-thread
        (thread (lambda ()
                  (let loop ()
                    (displayln (channel-get result-channel))
                    (loop)))))
 
(define work-channel (make-channel))
(define (make-worker thread-id)
  (thread
   (lambda ()
     (let loop ()
       (define item (channel-get work-channel))
       (case item
         [(DONE)
          (channel-put result-channel
                       (format "Thread ~a done" thread-id))]
         [else
          (channel-put result-channel
                       (format "Thread ~a processed ~a"
                               thread-id
                               item))
          (loop)])))))
(define work-threads (map make-worker '(1 2)))
(for ([item '(A B C D E F G H DONE DONE)])
  (channel-put work-channel item))
(for-each thread-wait work-threads)

;|#


#|

(require racket/async-channel)
 
(define print-thread
  (thread (lambda ()
            (let loop ()
              (displayln (thread-receive))
              (loop)))))
(define (safer-printf . items)
  (thread-send print-thread
               (apply format items)))
 
(define work-channel (make-async-channel 3))
(define (make-worker-thread thread-id)
  (thread
   (lambda ()
     (let loop ()
       (define item (async-channel-get work-channel))
       (safer-printf "Thread ~a processing item: ~a" thread-id item)
       (loop)))))
 
(for-each make-worker-thread '(1 2 3))
(for ([item '(a b c d e f g h i j k l m)])
  (async-channel-put work-channel item))

|#


(define main-thread (current-thread))
(define alarm (alarm-evt (+ 3000 (current-inexact-milliseconds))))
(define channel (make-channel))
(define (make-worker-thread thread-id)
  (thread
   (lambda ()
     (define evt (sync channel alarm))
     (cond
       [(equal? evt alarm)
        (thread-send main-thread 'alarm)]
       [else
        (thread-send main-thread
                     (format "Thread ~a received ~a"
                             thread-id
                             evt))]))))
(make-worker-thread 1)
(make-worker-thread 2)
(make-worker-thread 3)
(channel-put channel 'A)
(channel-put channel 'B)
(let loop ()
  (match (thread-receive)
    ['alarm
     (displayln "Done")]
    [result
     (displayln result)
     (loop)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (serve in-port out-port)
  (let loop []
    (define evt (sync/timeout 2
                              (read-line-evt in-port 'any)
                              (thread-receive-evt)))
    (cond
      [(not evt)
       (displayln "Timed out, exiting")
       (tcp-abandon-port in-port)
       (tcp-abandon-port out-port)]
      [(string? evt)
       (fprintf out-port "~a~n" evt)
       (flush-output out-port)
       (loop)]
      [else
       (printf "Received a message in mailbox: ~a~n"
               (thread-receive))
       (loop)])))

(define port-num 4321)
(define (start-server)
  (define listener (tcp-listen port-num))
  (thread
    (lambda ()
      (define-values [in-port out-port] (tcp-accept listener))
      (serve in-port out-port))))
 
(start-server)
 
(define client-thread
  (thread
   (lambda ()
     (define-values [in-port out-port] (tcp-connect "localhost" port-num))
     (display "first\nsecond\nthird\n" out-port)
     (flush-output out-port)
     ; copy-port will block until EOF is read from in-port
     (copy-port in-port (current-output-port)))))
 
(thread-wait client-thread)

