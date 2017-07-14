(define Ns '())
(define Rs '())

(define member?
  (lambda (a lat)
    (cond ((null? lat) #f)
          (else (or (eq? (car lat) a)
                    (member? a (cdr lat)))))))

(define deep
  (lambda (m)
    (cond ((= m 1) '(pizza))
          (else (cons (deepM (- m 1))
                      '())))))

(define deepR
  (lambda (n)
    (let ((result (deep n)))
      (set! Rs (cons (deep n) Rs))
      (set! Ns (cons n Ns))
      result)))

(define find
  (lambda (n Ns Rs)
    (letrec
        ((A (lambda (ns rs)
              (cond ((null? ns) #f)
                    ((= (car ns) n) (car rs))
                    (else (A (cdr ns) (cdr rs)))))))
      (A Ns Rs))))

(define deepM
  (let ((Rs '())
        (Ns '()))
    (lambda (n)
      (let ((exists (find n Ns Rs)))
        (if (not (pair? exists))
            (let ((result (deep n)))
              (set! Rs (cons result Rs))
              (set! Ns (cons n Ns))
              result)
            exists)))))

(define len
  (let ((h (lambda (l) 0)))
    (set! h
          (lambda (l)
            (cond ((null? l) 0)
                  (else (+ 1 (h (cdr l)))))))
    h))

(define D
  (lambda (depth*)
    (lambda (s)
      (cond ((null? s) 1)
            ((not(pair? (car s)))
             (depth* (cdr s)))
            (else (max
                   (+ 1 (depth* (car s)))
                   (depth* (cdr s))))))))



