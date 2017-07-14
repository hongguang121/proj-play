#lang typed/racket

(: check (-> (Listof Any) (Listof Any)))
(define (check ls)
  (let loop ((ls ls)
             (res '()))
    (if (null? ls)
        (reverse res)
        (loop (filter-not (λ (e) (eq? e (car ls))) (cdr ls))
              (cons (car ls) res)))))

(: fact (-> Number Number))
(define/match (fact n)
  [(0) 1]
  [(n) (* n (fact (sub1 n)))])