(define (len item)
  (define (length-iter item count)
    (if (null? item)
        count
        (length-iter (cdr item) (+ count 1))))
  (length-iter item 0))

(define (item arr index)
  (if (= index 1)
      (car arr)
      (item (cdr arr) (- index 1))))

(define (int n)
  (if (= 1 (remainder n 2))
      (+ n 1)
      n))

(define (bds arr num)
  (define (bds-iter first mid last)
    (cond ((> (item arr mid) num) (bds-iter first (/ (int mid) 2) mid))
          ((< (item arr mid) num) (bds-iter (+ mid 1) (/ (int (+ last mid)) 2) last))
          (else mid)))
  (bds-iter 1 (/ (int (len arr)) 2) (len arr)))