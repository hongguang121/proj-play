#lang racket

(define union
    (λ (set1 set2)
      (letrec
          ((U
            (λ (set)
              (cond ((null? set) set2)
                    ((M? (car set) set2)
                     (U (cdr set)))
                    (else (cons (car set)
                                (U (cdr set)))))))
           (M?
            (λ (a lat)
              (cond ((null? lat) #f)
                    ((eq? (car lat) a) #t)
                    (else (M? a (cdr lat)))))))
        (U set1))))

