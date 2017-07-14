(define (last l)
  (define (last-iter s n)
    (if (= n 1)
        (car s)
        (last-iter (cdr s) (- n 1))))
  (last-iter l (length l)))

(define (no-last l)
  (define (iter s n)
    (if (= n 0)
        '()
        (cons (car s) 
              (iter (cdr s) (- n 1)))))
  (iter l (- (length l) 1)))

(define (q-sort s)
  (if (< (length s) 2)
      s
      (append
       (q-sort (filter 
                (lambda (x)
                  (< x (last s)))
                s))
       (filter (lambda (x)
                 (= x (last s)))
               s)
       (q-sort (filter 
                (lambda (x)
                  (> x (last s)))
                s)))))

(define rember
  (lambda (a lat)
    (cond ((null? lat) '())
          (else (cond ((eq? a (car lat)) (cdr lat))
                      (else (cons (car lat)
                                  (rember a (cdr lat)))))))))

(define (bubble-sort s)
  (define (iter l result)
    (if (null? l)
        result
        (iter (rember (apply max l) l)
              (cons (apply max l) result))))
  (iter s '()))

(define div
  (lambda (n m)
    (if (zero? m) 
        'error
        (cond ((< n m) 0)
              (else (add1 (div (- n m) m)))))))

(define (cut str start end)
  (define (iter add count s r)
    (cond ((> start end) 
           (if (not (< add end))
               (if (not (< count (+ (- start end) 1)))
                   r
                   (iter add (+ count 1) (cdr s) 
                         (cons (car s) r)))
               (iter (+ add 1) count (cdr s) r)))
          ((not (> start end)) 
           (if (not (< add start))
               (if (not (< count (+ (- end start) 1)))
                   '()
                   (cons (car s)
                         (iter add (+ count 1) (cdr s) r)))
               (iter (+ add 1) count (cdr s) r)))))
  (iter 0 0 str '()))

(define (merge left right)
  (cond ((null? left) right)
        ((null? right) left)
        (else 
         (let ((l (car left)) (r (car right)))
           (cond ((< l r) (cons l 
                                (merge (cdr left) right)))
                 ((> l r) (cons r
                                (merge left (cdr right))))
                 ((= l r) (cons l
                                (merge (cdr left) right))))))))

(define (merge-sort s)
  (if (= (length s) 1)
      s
      (let ((mid (div (length s) 2)))
        (let ((left (cut s 0 (- mid 1)))
              (right (cut s mid (- (length s) 1))))
          (merge (merge-sort left) 
                 (merge-sort right))))))

(define insertR 
  (lambda (new old lat)
    (cond ((null? lat) '())
          (else (cond 
                  ((eq? old (car lat)) (cons old
                                             (cons new 
                                                   (insertR new old (cdr lat)))))
                      (else (cons (car lat)
                                  (insertR new old (cdr lat)))))))))

(define insertL
  (lambda (new old lat)
    (cond ((null? lat) '())
          (else (cond 
                  ((eq? old (car lat)) (cons new
                                             (cons old 
                                                   (insertL new old (cdr lat)))))
                      (else (cons (car lat)
                                  (insertL new old (cdr lat)))))))))

(define (insert-sort s)
  (define (iter new)
    (cond ((null? new) '())
          ()))