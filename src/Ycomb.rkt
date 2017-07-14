(define l '(a b c d e f g))
(define lt '(1 2 3 4 5 6 7 8 9 10))

(define eternity
    (lambda (l)
      (if (null? l)
          0
          (+ 1 (eternity (cdr l))))))

(((lambda (length)
    (lambda (l)
      (if (null? l)
          0
          (+ 1 (length (cdr l))))))
  eternity)
 l)

((lambda(l)
    (cond ((null? l) 0)
          (else (+ 1 (eternity (cdr l))))))
   l)

(((lambda (f)
    (lambda (l)
      (cond ((null? l) 0)
            (else (+ 1 (f (cdr l)))))))
   ((lambda (g)
      (lambda (l)
        (cond ((null? l) 0)
              (else (+ 1 (g (cdr l)))))))
    eternity)) l)

(define fact
    (lambda (n)
      (if (= n 0)
          1
          (* n (fact (- n 1))))))

(define fact-gen
  (lambda (func)
    (lambda (n)
      (if (= n 0)
          1
          (* n (func (- n 1)))))))

(define fib-gen
  (lambda (func)
    (lambda (n)
      (if (< n 2)
          n
          (+ (func (- n 1))
             (func (- n 2)))))))

(((lambda (fact)
     (lambda (n)
       (cond ((= n 0) 1)
             (else (* n (fact (- n 1)))))))
     fact) 6)

(((lambda (mk-length)
    (mk-length
     (mk-length
      (mk-length eternity))))
  (lambda (length)
    (lambda (l)
      (cond ((null? l) 0)
            (else (+ 1 (length (cdr l))))))))
 l)


(((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond ((null? l) 0)
           (else (+ 1 ((mk-length eternity)
                       (cdr l))))))))
 '(apple))

(((lambda (mk-length)
   (mk-length mk-length))
  (lambda (mk-length)
    (lambda (l)
      (cond ((null? l) 0)
            (else (+ 1 ((mk-length mk-length)
                        (cdr l))))))))
 l)

(lambda (le)
  ((lambda (mk-length)
     (mk-length mk-length))
   (lambda (mk-length)
     (le (lambda (x)
           ((mk-length mk-length) x))))))

(define Y
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
       (le (lambda (x) ((f f) x)))))))

(define sum
  (lambda (lt)
    (if (null? lt)
        0
        (+ (car lt)
           (sum (cdr lt))))))

(define sum-seq-fn-gen
  (lambda (func)
    (lambda (s)
      (if (null? s)
          0
          (+ (car s)
             (func (cdr s)))))))

