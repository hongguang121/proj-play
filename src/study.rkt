(require math)

(define atom?
    (lambda (x)
      (and (not (pair? x)) (not (null? x)))))

(define lat?
  (lambda (l)
    (cond ((null? l) #t)
          ((atom? (car l)) (lat? (cdr l)))
          (else #f))))

(define member?
  (lambda (a lat)
    (cond ((null? lat) #f)
          (else (or (eq? (car lat) a)
                    (member? a (cdr lat)))))))

(define rember
  (lambda (a lat)
    (cond ((null? lat) (quote()))
          (else (cond ((eq? (car lat) a) (cdr lat))
                      (else (rember a (cdr lat))))))))

(define rember-f
  (lambda (a lat)
    (cond ((null? lat) (quote()))
    (else (cons (car lat) (rember-f a (cdr lat)))))))

(define firsts
  (lambda (l)
    (cond 
      ((null? l) '())
      (else (cons (car (car l))
                  (firsts (cdr l)))))))

(define (square x) (* x x))
(define (cube x) (* x x x))

(define (power a b)
  (if (< b 0) 
      (/ 1 (power-iter 1 a b))
      (power-iter 1 a b)))
       
(define (power-iter n a b)
  (if (< (abs b) 1)
      n
      (power-iter (* n a) a (- (abs b) 1))))

(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum (+ a 1) b))))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (inc n) (+ n 1))
(define (sum-cubes a b)
  (sum cube a inc b))

(define (f x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))

(define (pi-sum a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
       a
       (lambda (x) (+ x 4))
       b))

(define (f a b)
    (if (> a b)
        (newline)
        ((display (/ (sin (power 10 (- 1 a))) a))
         (newline)
         (f (+ a 1) b))))

(define (listref items n)
  (if (= n 0)
      (car items)
      (listref (cdr items) (- n 1))))

(define (length-f items)
  (if (null? items)
      0
      (+ 1 (length-f (cdr items)))))

(define (length-t items)
  (define (length-iter a count)
    (if (null? a)
        count
        (length-iter (cdr a) (+ 1 count))))
  (length-iter items 0))

(define (last-pair-f l)
  (last-pair-iter l (length l)))
(define (last-pair-iter l count)
      (list-ref l (- count 1)))