#lang racket

(define test
  (thread
   (lambda ()
     (display "Thread is start...")
     (sleep 2)
     (display "Thread is running...")
     (sleep 2))))

(define (fact n)
  (if (= n 0)
      1
      (* n (fact (- n 1)))))

(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))


(thread-wait test)
(kill-thread test)

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

(define output-semaphore (make-semaphore 1))
(define (make-thread name)
  (thread (lambda ()
            (for [(i 10)]
              (call-with-semaphore
               output-semaphore
               (lambda ()
                (printf "thread ~a: ~a~n" name i)))))))
(define threads
  (map make-thread '(a b c)))
(for-each thread-wait threads)

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

;(apply string-append (map (lambda (x) (make-string 1 x)) (string->list "Hello World")))

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