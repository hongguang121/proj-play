#lang plai

(define-type NumTree
  [mt]
  [node (v number?)
        (l NumTree?)
        (r NumTree?)])

(define-type MisspelledAnimal
  [caml (humps number?)]
  [yacc (height number?)])

(define ma1 (caml 2))
(define ma2 (yacc 1.9))

(define (good? [ma MisspelledAnimal?]) boolean?
  (type-case MisspelledAnimal ma
    [caml (humps) (>= humps 2)]
    [yacc (height) (> height 2.1)]))

(define (good+? ma)
  (cond [(caml? ma) (>= (caml-humps ma) 2)]
        [(yacc? ma) (>= (yacc-height ma) 2.1)]))

(define-type ArithC
  [numC (n number?)]
  [plusC (l ArithC?) (r ArithC?)]
  [multC (l ArithC?) (r ArithC?)])

(define (parse [s symbol?]) ArithC?
  (cond [(number? s) (numC s)]
        [(list? s)
         (let ([sl s])
           (case (car sl)
             [(+) (plusC (parse (cadr sl)) (parse (caddr sl)))]
             [(*) (multC (parse (cadr sl)) (parse (caddr sl)))]
             [else (error 'parse "invalid list input")]))]))

