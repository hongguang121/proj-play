(define (sp a b)
  (list b a))

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
      (- n 1)
      n))

(define (div-sort arr)  
  (define (div-sort-iter left mid right)
    (define (sort l m r)
      (if (> l mid)
          right
          (if (> m right)
              (sort (+ l 1) m r)
              (if (> (item arr l) (item arr (+ m 1)))
                  ((swap l (+ m 1))
                   (sort l (+ m 1) r))
                  (sort (+ l 1) m r)))))
    (if (or (= (- mid left) 0) (= (- right mid) 1))
        (sort left mid right)
        (merge left
               (div-sort-iter left (/ (int (+ left mid)) 2) mid)
               (div-sort-iter (+ mid 1) (/ (int (+ mid right)) 2) right))))
  
  (define (merge left mid right)
    (if (= right (len arr))
        arr    
        right))
    
  (div-sort-iter 1 (/ (int (len arr)) 2) (len arr)))