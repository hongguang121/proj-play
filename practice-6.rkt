#lang typed/racket

(: Seasoning (-> Symbol Symbol))
(define (Seasoning type)
  (define (Salt)
    'Salt)
  (define (Pepper)
    'Pepper)
  (define (Select)
    (match type
      ['Salt (Salt)]
      ['Pepper (Pepper)]))
  (Select))