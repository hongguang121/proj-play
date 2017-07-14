#lang racket

(define (list=? lr lt)
  (and (null? (filter-not (λ (x) (member x lt)) lr))
       (null? (filter-not (λ (x) (member x lr)) lt))))

(define (list->alone ls)
  (let ((ls (for/list ((i ls))
              (list i))))
    (cons '() ls)))

(define (list->couple ls)
  (if (null? ls)
      '()
      (append 
       (let-values (((lr lt) (split-at ls 1)))
         (for/list ((i lt))
           (cons (car lr) (list i))))
       (list->couple (cdr ls)))))

(define (combinate ls curr)
  (remove-duplicates
   (apply append
          (map
           (λ (x)
             (map (λ (y) (cons x y))
                  (filter-not (λ (e) (member x e)) curr)))
           ls))
   list=?))

(define (list->all ls)
  (let ((len (length ls)))
    (let loop ((res (combinate ls (list->alone ls)))
               (rem '()))      
      (cond ((>= (apply max (map length res)) (sub1 len))
             (cons '() (remove-duplicates (append res rem (list ls)) list=?)))
            (else (loop (combinate ls res) (append res rem)))))))