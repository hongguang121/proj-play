(define (sum num)
  (let f ([i 0] [s 0])
    (if (> i num)
        s
        (f (+ i 1) (+ s i)))))