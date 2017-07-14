#lang racket

(define eternity
  (lambda (x)
    (eternity x)))

(define Y
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
       (le (lambda (x) ((f f) x)))))))

(define l '(a b c))

((Y (lambda (len)
       (lambda (l)
         (cond ((null? l) 0)
               (else (+ 1 (len (cdr l)))))))) '(1 2 3))

(define len
  (Y (lambda (len)
       (lambda (l)
         (cond ((null? l) 0)
               (else (+ 1 (len (cdr l)))))))))

(len '(1 2 3))

(((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (+ 1
                ((mk-length mk-length)
                 (cdr l)))))))
 ) 
 '(1 2 3))

(
 ((lambda (fact)
    (fact fact))
  (lambda (fact)
    (lambda (n)
      (cond
        ((= n 0) 1)
        (else (* n ((fact fact)
                    (- n 1))))))))
 6)


;(lambda (mk-length)
;   (mk-length mk-length)) 展开如下：

(
 ((lambda (mk-length)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (+ 1
                 ((mk-length eternity)
                  (cdr l)))))))
  (lambda (mk-length)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (+ 1
                 ((mk-length eternity)
                  (cdr l))))))))
 '(apple))

(define fac
  (
   (lambda (f)
     (lambda (n)
       (lambda (result)
         (if (= n 1)
             result
             (((f f) (- n 1)) (* n result))))
       ))
   (lambda (f)
     (lambda (n)
       (lambda (result)
         (if (= n 1)
             result
             (((f f) (- n 1)) (* n result))))
       ))
   )
  )

(define (fact n)
  ((lambda (f)
     (((f f) n) 1))
   (lambda (f)
     (lambda (n)
       (lambda (result)
         (if (= n 1)
             result
             (((f f) (- n 1)) (* n result))))))))