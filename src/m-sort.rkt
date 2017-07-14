(define (find-add lst n)
  (define (find-add-iter arr count)
    (if (null? arr) 
        '()
        (if (= (- count 1) n)
            (cons 'D 
                   (find-add-iter arr (+ count 1)))
            (cons (car arr) 
                  (find-add-iter (cdr arr) (+ count 1))))))
(find-add-iter lst 0))

(define (int n)
  (if (= 1 (remainder n 2))
      (+ n 1)
      n))

(define m-sort
  ())

(define (compare lst)
    (let ([x (car lst)]
          [y (cadr lst)])
      (if (> x y)
          (list y x)
          lst)))

