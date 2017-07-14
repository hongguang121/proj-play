#lang racket

(define (len item)
  (define (len-iter a count)
    (if (null? a)
        count
        (len-iter (cdr a) (+ 1 count))))
  (len-iter item 0)) 

(define (lp arr)
  (define (lp-iter arr n)
    (if (= n 0)
        (car arr)
        (lp-iter (cdr arr) (- n 1))))
  (lp-iter arr (- (len arr) 1)))

(define (revs arr)
  (define (revs-iter arr result)
    (if (null? arr)
        result
        (revs-iter (cdr arr) 
                   (cons (car arr) result))))
  (revs-iter arr '()))

(define (fact-tail n)
  (letrec ((R (λ (n s)
                (cond ((= n 0) s)
                      (else (R (- n 1) (* n s)))))))
    (R n 1)))

(define (fib-tail n)
  (letrec ((R (λ (a b cnt)
                (cond ((= cnt n) a)
                      (else (R (+ a b) a (+ cnt 1)))))))
    (R 1 0 1)))

(define (fact-cps n ret)
  (cond ((= n 0) (ret 1))
        (else (fact-cps (- n 1)
                        (λ (t) (ret (* n t)))))))
;(fact-cps 6 (λ (x) x))

(define (fact-tail-cps n s ret)
  (cond ((= n 0) (ret s))
        (else (fact-tail-cps (- n 1)
                             (* n s)
                             ret))))
;(fact-tail-cps 6 1 (λ (x) x))
 

(for*/list ([i '(a b c)]
              [n '(1 2 3)])
    (format "~a: ~a" i n))

;x选c l:'()
(define (m-list x c l)
  (let ((n (random x)))
    (if (= 0 c) l
        (if (and (eq? (member n l) #f)(not (= n 0)))
            (m-list x (- c 1) (cons n l))
            (m-list x c l)))))

;(filter (λ (x) (and (> x 0)(< x 36))) (m-list 7))

(define (qsort l [cmp <] [key (lambda (x) x)])
  (if (or (empty? l) (empty? (cdr l))) l
      (let*-values ([(item) (car l)]
                    [(small big)
                     (partition (lambda (x) (cmp (key x) (key item)))
                                (cdr l))])
        (append (qsort small cmp key)
                (list item)
                (qsort big cmp key)))))

(define add
  (lambda args
    (let loop ([ls args])
      (if (null? ls) 0
          (let ([arg (car ls)])
            (+ arg (loop (cdr ls))))))))