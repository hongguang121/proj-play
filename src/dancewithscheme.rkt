(define 1+ (lambda (x) (+ 1 x)))

(define (same? x y)
  (not (or (> x y) (< x y))))

(define (sum-of-squares numbers)
  (apply + (map (lambda (x) (* x x)) numbers)))

(define (dot-product left-vector right-vector)
  (apply + (map (lambda (x y) (* x y)) left-vector right-vector)))

(define (transpose matrix)
  (apply map (cons list matrix)))

(define (tree-depth tree)
  (cond
    ((list? tree) (+ 1 (apply max (map tree-depth tree))))
    (else 0)))

(define-syntax du
  (syntax-rules ()
    ((_ expression unless condition)
     (if (not condition) expression))))

(define (flat-map f xs)
  (apply append (map f xs)))

(define-syntax list-of
  (syntax-rules (<-)
    ((_ expr (v <- aList) rule ...)(flat-map (lambda(v) (list-of expr rule ...)) aList))
    ((_ expr filter rule ...)(if filter (list-of expr rule ...) '()))
    ((_ expr) (cons expr '()))))

(define-syntax def
  (syntax-rules ()
    [(_ ((x e) ...) b1 b2 ...)
     ((lambda (x ...) b1 b2 ...) e ...)]))


;(define-macro when
;  (lambda (test . branch)
;    (list 'if test
;          (cons 'begin branch))))