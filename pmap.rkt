#lang racket

(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1))
         (fib (- n 2)))))

(define start (current-milliseconds))

(define output-semaphore (make-semaphore 1))

(define res '())

(define pmap-channel (make-channel))

(define (pmap thread-id)
  (thread
   (λ ()
     (call-with-semaphore
      output-semaphore
      (λ ()
        (let loop ()       
          (define e (channel-get pmap-channel))
          (case e
            ((done) res)
            (else          
             (set! res (cons (map fib (list e)) res))
             (loop)))))))))

(define pmap-threads (map pmap '(1 2)))

(define (compute lst)
      (for ((e lst))
        (channel-put pmap-channel e)))

(define ls '(30 36 35 33 34 31))

(compute (append ls '(done done)))

(for-each thread-wait pmap-threads)

(define end (current-milliseconds))

(printf "Time: ~a\n" (- end start))