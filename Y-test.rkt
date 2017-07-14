#lang racket

(define eternity
  (λ (x)
    (eternity x)))

(
 (lambda (l)
  (cond ((null? l) 0)
        (else (+ 1
                 ((lambda (l)
                   (cond ((null? l) 0)
                         (else (+ 1
                                  (eternity (cdr l))))))
                 (cdr l))))))
 '(1))

(
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond ((null? l) 0)
           (else (+ 1
                    ((mk-length mk-length)
                     (cdr l))))))))
'(1 2 3))

(
 ((lambda (mk-fact)
    (mk-fact mk-fact))  
  (lambda (fact)
     (lambda (n)
       (cond ((= n 0) 1)
             (else (* n
                      ((fact fact) (- n 1))))))))
 6)


(define Y
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
       (le (lambda (x) ((f f) x)))))))

((Y (lambda (length)
       (lambda (l)
         (cond ((null? l) 0)
               (else (+ 1 (length (cdr l)))))))) '(1 2 3))