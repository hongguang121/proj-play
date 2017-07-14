(define (sum+ x y)
  (if (= x 0)
      y
      (sum+ (- x 1) (+ y 1))))
(define (sum++ x y)
  (if (= x 0)
      y
      (+ 1 (+ (- x 1) y))))

(define (hant n) 
  (if (< n 2)
      1
      (+ 1 (* 2 (hant (- n 1)))))
  )
(define (hant+ n count over)
  (if (= count over)
      n
      (hant+ (+ (* 2 n) 1) (+ 1 count) over)))
 