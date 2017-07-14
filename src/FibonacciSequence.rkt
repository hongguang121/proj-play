(define (fib n)
    (cond ((= n 0) 0)
          ((= n 1) 1)
    (else (+ (fib(- n 1)) (fib(- n 2))))))

(define (fib+ n)
    (f 0 1 n))
(define (f a b n)
    (if (= n 0) 
        a
        (f b (+ a b) (- n 1))))