#lang typed/racket

(define-type IB (U Integer Boolean))

(: id (-> IB IB))
(define (id x) x)

(: foo (-> (-> IB IB) (values IB IB)))
(define (foo f)
  (values (f 1) (f #t)))

(struct None ())
(struct (a) Some ([v : a]))
 
(define-type (Opt a) (U None (Some a)))
 
(: find (-> Number (Listof Number) (Opt Number)))
(define (find v l)
  (cond [(null? l) (None)]
        [(= v (car l)) (Some v)]
        [else (find v (cdr l))]))

(define ls '((apple 300 red) (pear 400 yellow) (banana 240 yellow) (orange 330 orange)))
