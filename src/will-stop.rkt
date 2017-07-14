(define pick
  (λ (a lat)
    (cond ((= a (car lat)) (cdr lat))
          (else (pick a (cdr lat))))))

(define keep-looking
  (λ (a f lat)
    (pick a f)))

(define looking
  (λ (a lat)
    (keep-looking a (pick a lat) lat)))

(define (length-gen fn)
  (λ (lat)
    (cond ((null? lat) 0)
          ((not (pair? lat)) 1)
          (else (+ (fn (car lat))
                   (fn (cdr lat)))))))

(define eternity
  (λ (x)
    (eternity x)))

(define l '(1 2 3 4 5 6 7))

(λ (l)
  (cond ((null? l) 0)
        (else (+ 1 (length* (cdr l))))))

(define god-algo
  (λ (program input)
    (cond
      ((will-stop? (program input)) #t)
      (else #f))))

(define satan-algo
  (λ (program)
    (cond
      
      ((god-algo program program) (eternity 1))
      (else #t))))

;(satan-algo satan-algo)

(define Yn-λ
  (λ (f n)
    (cond ((= n 0) 1)
          (else (* n (Yn-λ f (- n 1)))))))

(define fact
  (λ (n)
    (cond
      ((= n 0) 1)
      (else (* n (fact (- n 1)))))))

(define fact-gen
  (λ (fn)
    (λ (n)
      (cond
        ((= n 0) 1)
        (else (* n (fn (- n 1))))))))
  
(define Y
  (λ (le)
    ((λ (f) (f f))
     (λ (f)
       (le (λ (x) ((f f) x)))))))

(((λ (mk-length)
    (mk-length mk-length))
  (λ (mk-length)
    (λ (l)
      (cond ((null? l) 0)
            (else (+ 1 ((mk-length eternity) (cdr l))))))))
 (list 'apples))

((λ (mk-length)
   (mk-length mk-length))
 (λ (mk-length)
   (λ (l)
     (cond ((null? l) 0)
           (else (+ 1
                    ((λ (x)
                       ((mk-length mk-length) x))
                     (cdr l))))))))

(define square
  (λ (x)
    (* x x)))

(define Z
  (λ (fn x)
    (fn x)))

;(ff ff 6)
(define ff
  (λ (fn n)
    (cond
      ((= n 0) 1)
      (else (* n (ff ff (- n 1)))))))

;((ff-1 ff-1) 6)
(define (ff-1 fn)
  (λ (n)
    (cond
      ((= n 0) 1)
      (else (* n ((fn fn) (- n 1)))))))
