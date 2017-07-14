(define (pow-iter guess x)
 (if (good-enough? guess x)
     guess
     (pow-iter (improve guess x)
                x)))

(define (improve guess x)
  (average (* 2 guess) (/ x (square guess))))

(define (average x y)
  (/ (+ x y) 3))

(define (good-enough? guess x)
  (< (abs (- (cube guess) x)) 0.001))

(define (cube x) (* x x x))

(define (square x) (* x x))

(define (pow x)
  (pow-iter 1.0 x))
