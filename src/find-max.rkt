#lang racket
(require racket/trace)
(define (col x)
  (cond
    ((> 0 x)x)
    ((even? x) (/ x 2))
    (else (add1(* 3 x)))))
(define (gene-col x ht)
  (cond
    ((number? (hash-ref ht x null)) (hash-ref ht x))
    ((= 1 x) (begin(hash-set! ht x 1)(hash-ref ht x)))
    (else (begin(hash-set! ht x (add1 (gene-col (col x) ht))) 
                (hash-ref ht x)))))
;;alop is a list of pairs
(define (find-cdr alop h)
  (if(= h (cdr(car alop)))
     (car alop)
     (find-cdr (cdr alop) h)))
(define ht (make-hash))
(define answers (map (lambda (x) (gene-col x ht))(build-list 1000000 (lambda (x) (add1 x)))))
(find-cdr (hash->list ht) (apply max answers))