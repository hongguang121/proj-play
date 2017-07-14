;; The Little Schemer

(define atom? 
  (lambda (e)
    (if (and (not (pair? e)) (not (null? e)))
        #t
        #f)))

(define lat?
  (lambda (l)
    (cond ((null? l) #t)
          ((atom? (car l)) (lat? (cdr l)))
          (else #f))))

(define member?
  (lambda (a lat)
    (cond ((null? lat) #f)
          (else (or (eq? a (car lat))
                    (member? a (cdr lat)))))))

(define (rember a lat)
  (cond ((null? lat) '())
        ((eq? a (car lat)) (rember a (cdr lat)))
        (else (cons (car lat)
                    (rember a (cdr lat))))))

(define rember+
  (lambda (a lat)
    (cond ((null? lat) '())
          (else (cond ((eq? a (car lat)) (cdr lat))
                      (else (cons (car lat)
                                  (rember a (cdr lat)))))))))

(define firsts 
  (lambda (l)
    (cond ((null? l) '())
          (else (cons (car (car l))
                      (firsts (cdr l)))))))
(define seconds
  (lambda (l)
    (cond ((null? l) '())
          (else (cons (car (cdr (car l)))
                      (seconds (cdr l)))))))

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

(define multisubst
  (lambda (new old lat)
    (cond ((null? lat) '())
          (else (cond ((eq? (car lat) old)
                       (cons new 
                             (multisubst new old (cdr lat))))
                      (else (cons (car lat)
                                  (multisubst new old (cdr lat)))))))))

;;(add1) (sub1) (+)

(define add
  (lambda (n m)
    (if (zero? m) 
        n
        (add1 (add n (sub1 m))))))

(define sub
  (lambda (n m)
    (if (zero? m)
        n
        (sub1 (sub n (sub1 m))))))

(define mul
  (lambda (n m)
    (if (zero? m) 
        0
        (add n (mul n (sub1 m))))))

(define div
  (lambda (n m)
    (if (zero? m) 
        'error
        (cond ((< n m) 0)
              (else (add1 (div (- n m) m)))))))

(define tup+
  (lambda (tup1 tup2)
    (cond ((null? tup1) tup2)
          ((null? tup2) tup1)
          (else (cons (add (car tup1) (car tup2))
                      (tup+ (cdr tup1) (cdr tup2)))))))

(define average
  (lambda (nums)
    (/ (apply + nums) (length nums))))

(define is-even?
  (lambda (n)
    (if (= n 0) #t
        (is-odd? (- n 1)))))
(define is-odd?
  (lambda (n)
    (if (= n 0) #f
        (is-even? (- n 1)))))

(define (power a b)
  (if (< b 0) 
      (/ 1 (power-iter 1 a b))
      (power-iter 1 a b)))

(define (power-iter n a b)
  (if (< (abs b) 1)
      n
      (power-iter (* n a) a (- (abs b) 1))))

(define cpy
  (lambda (l)
    (if (null? l)
        '()
        (cons (car l)
              (cpy (cdr l))))))

(define len
  (lambda (l)
    (if (null? l)
        0
        (add1 (len (cdr l))))))

(define pick
  (lambda (n l)
    (if (zero? n) 
        (car l)
        (pick (- n 1) (cdr l)))))

(define rempick
  (lambda (l n)
    (if (zero? n)
        (cdr l)
        (cons (car l)
              (rempick (cdr l) (- n 1))))))

(define no-nums
  (lambda (l)
    (cond ((null? l) '())
          ((number? (car l)) (no-nums (cdr l)))
          (else (cons (car l)
                      (no-nums (cdr l)))))))

(define all-nums
  (lambda (lat)
    (cond ((null? lat) '())
          (else (cond ((number? (car lat))
                       (cons (car lat)
                             (all-nums (cdr lat))))
                      (else (all-nums (cdr lat))))))))

(define occur
  (lambda (a lat)
    (cond ((null? lat) 0)
          (else (cond ((eq? (car lat) a)
                       (add1 (occur a (cdr lat))))
                       (else (occur a (cdr lat))))))))

(define rember*
  (lambda (a l)
    (cond ((null? l) '())
          ((atom? (car l))
           (cond ((eq? (car l) a)
                  (rember* a (cdr l)))
                 (else (cons (car l)
                             (rember* a (cdr l))))))
          (else (cons (rember* a (car l))
                      (rember* a (cdr l)))))))

(define insertR*
  (lambda (new old lat)
    (cond ((null? lat) '())
          ((atom? (car lat))
           (cond ((eq? (car lat) old)
                  (cons old
                        (cons new
                              (insertR* new old (cdr lat)))))
                 (else (cons (car lat)
                             (insertR* new old (cdr lat))))))
          (else (cons (insertR* new old (car lat))
                      (insertR* new old (cdr lat)))))))

(define occur*
  (lambda (a lat)
    (cond ((null? lat) 0)
          ((atom? (car lat))
           (cond ((eq? a (car lat))
                  (add1 (occur* a (cdr lat))))
                 (else (occur* a (cdr lat)))))
          (else (add (occur* a (car lat))
                     (occur* a (cdr lat)))))))

(define subst*
  (lambda (new old lat)
    (cond ((null? lat) '())
          ((atom? (car lat))
           (cond ((eq? (car lat) old)
                  (cons new (subst* new old (cdr lat))))
                  (else (cons (car lat)
                              (subst* new old (cdr lat))))))
          (else (cons (subst* new old (car lat))
                      (subst* new old (cdr lat)))))))

;;(define l '((banana) (split ((((banana ice))) (cream (banana)) sherbet)) (banana) (bread) (banana brandy)))
;;(define a 'banana)

(define insertL*
  (lambda (new old lat)
    (cond ((null? lat) '())
          ((atom? (car lat))
           (cond ((eq? (car lat) old)
                  (cons new
                        (cons old (insertL* new old (cdr lat)))))
                 (else (cons (car lat)
                             (insertL* new old (cdr lat))))))
          (else (cons (insertL* new old (car lat))
                      (insertL* new old (cdr lat)))))))

(define member*
  (lambda (a l)
    (cond ((null? l) #f)
          ((atom? (car l))
           (or (eq? (car l) a)
               (member* a (cdr l))))
          (else (or (member* a (car l))
                    (member* a (cdr l)))))))

(define eqan?
  (lambda (a1 a2)
    (cond
      ((and (number? a1) (number? a2)) (= a1 a2))
      ((or (number? a1) (number? a2)) #f)
      (else (eq? a1 a2)))))


(define eqlist?
  (lambda (l1 l2)
    (cond ((and (null? l1) (null? l2)) #t)
          ((or (null? l1) (null? l2)) #f)
          ((and (atom? (car l1)) (atom? (car l2)))
           (and (eqan? (car l1) (car l2))
           (eqlist? (cdr l1) (cdr l2))))
          ((or (atom? (car l1)) (atom? (car l2))) #f)
          (else (and (eqlist? (car l1) (car l2))
                     (eqlist? (cdr l1) (cdr l2)))))))

(define equal??
  (lambda (s1 s2)
    (cond ((and (atom? s1) (atom? s2)) (eqan? s1 s2))
          ((or (atom? s1) (atom? s2)) #f)
          (else (eqlist? s1 s2)))))

(define eqlist??
  (lambda (l1 l2)
    (cond ((and (null? l1) (null? l2)) #t)
          ((or (null? l1) (null? l2)) #f)
          (else (and (equal? (car l1) (car l2))
                     (eqlist?? (car l1) (cdr l2)))))))

(define numbered?
  (lambda (aexp)
    (cond ((atom? aexp) (number? aexp))
          (else (and (numbered? (car aexp))
                     (numbered? (car (cdr (cdr aexp)))))))))

(define 1st-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))

(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(define operator
  (lambda (aexp)
    (car aexp)))

(define value
  (lambda (nexp)
    (cond ((atom? nexp) nexp)
          ((eq? (operator nexp) '(+))
           (+ (value (1st-sub-exp nexp))
              (value (2nd-sub-exp nexp))))
          ((eq? (operator nexp) '(*))
           (* (value (1st-sub-exp nexp))
              (value (2nd-sub-exp nexp))))
          (else (power (value (1st-sub-exp nexp))
                       (value (2nd-sub-exp nexp)))))))

(define sero?
  (lambda (n)
    (null? n)))

(define edd1
  (lambda (n)
    (cons (quote()) n)))

(define zub1
  (lambda (n)
    (cdr n)))

(define plus
  (lambda (n m)
    (cond ((sero? m) n)
          (else (edd1 (plus n (zub1 m)))))))

(define lat
  (lambda (l)
    (cond ((null? l) #t)
          ((atom? (car l)) (lat (cdr l)))
          (else #f))))

(define set?
  (lambda (lat)
    (cond ((null? lat) #t)
          ((member? (car lat) (cdr lat)) #f)
          (else (set? (cdr lat))))))

(define makeset
  (lambda (lat)
    (cond ((null? lat) '())
          (else (cons (car lat)
                      (makeset
                       (rember* (car lat)
                                (cdr lat))))))))

(define subset?
  (lambda (set1 set2)
    (cond ((null? set1) #t)
          (else (and (member? (car set1) set2)
                     (subset? (cdr set1) set2))))))

(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2)
         (subset? set2 set1))))

(define intersect?
  (lambda (set1 set2)
    (cond ((null? set1) #f)
          (else (or (member? (car set1) set2)
                    (interset? (cdr set1) set2))))))

(define intersect
  (lambda (set1 set2)
    (cond ((null? set1) (quote()))
          ((member? (car set1) set2)
           (cons (car set1)
                 (intersect (cdr set1) set2)))
          (else (intersect (cdr set1) set2)))))

(define union
  (lambda (set1 set2)
    (cond ((null? set1) set2)
          ((member? (car set1) set2)
           (union (cdr set1) set2))
          (else (cons (car set1)
                      (union (cdr set1) set2))))))

(define xxx
  (lambda (set1 set2)
    (cond ((null? set1) (quote()))
          ((member? (car set1) set2)
           (xxx (cdr set1) set2))
          (else (cons (car set1)
                      (xxx (cdr set1) set2))))))

(define intersectall
  (lambda (l-set)
    (cond ((null? (cdr l-set)) (car l-set))
          (else (intersect (car l-set)
                           (intersectall (cdr l-set)))))))

(define a-pair?
  (lambda (x)
    (cond ((atom? x) #f)
          ((null? x) #f)
          ((null? (cdr x)) #f)
          ((null? (cdr (cdr x))) #t)
          (else #f))))

(define build
  (lambda (s1 s2)
    (cond 
      (else (cons s1 
                  (cons s2 (quote())))))))

;;(define s1 '(2 5 7 6 9 3))
;;(define s2 '(9 8 0 1 4 3))
;;(define s3 '((a b c) (c a d e) (e f g h a b)))

(define fun?
  (lambda (rel)
    (set? (firsts rel))))

(define revrel
  (lambda (rel)
    (cond ((null? rel) (quote()))
          (else (cons (build (second (car rel))
                             (first (car rel)))
                      (revrel (cdr rel)))))))

(define fullfun?
  (lambda (fun)
    (set? (seconds fun))))

(define one-to-one?
  (lambda (fun)
    (fun? (revrel fun))))

(define rember-f
  (lambda (test? a l)
    (cond ((null? l) (quote()))
          ((test? (car l) a) (cdr l))
          (else (cons (car l)
                      (rember-f test? a (cdr l)))))))

(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))

;(define x 'salad)
;(define y 'tuna)
;(define eq?-salad (eq?-c x))
;(eq?-salad x)
;((eq?-c x) y)

(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond ((null? l) (quote ()))
            ((test? (car l) a) (cdr l))
            (else (cons (car l)
                        ((rember-f test?) a (cdr l))))))))

;(define test? 'eq?)
;(define rember-eq? (rember-f test?))

(define insertL-f
  (lambda (test?)
    (lambda (new old l)
      (cond ((null? l) (quote ()))
            ((test? (car l) old)
             (cons new (cons old (cdr l))))
            (else (cons (car l)
                        ((insertL-f test?) new old
                                           (cdr l))))))))

(define insertR-f
  (lambda (test?)
    (lambda (new old l)
      (cond ((null? l) (quote ()))
            ((test? (car l) old)
             (cons old (cons new (cdr l))))
            (else (cons (car l)
                        ((insertR-f test?) new old
                                           (cdr l))))))))

(define seqL
  (lambda (new old l)
    (cons new (cons old l))))

(define seqR 
  (lambda (new old l)
    (cons old (cons new l))))

(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond ((null? l) (quote()))
            ((eq? (car l) old)
             (seq new old (cdr l)))
            (else (cons (car l)
                        ((insert-g seq) new old
                                        (cdr l))))))))
;(define insertL (insert-g seqL))
;(define insertR (insert-g seqR))

(define insertL
  (insert-g
   (lambda (new old l)
     (cons new (cons old l)))))

(define subst
  (lambda (new old l)
    (cond ((null? l) (quote ()))
          ((eq? (car l) old)
           (cons new (cdr l)))
          (else (cons (car l)
                      (subst new old (cdr l)))))))

(define seqS
  (lambda (new old l)
    (cons new l)))

(define subst (insert-g seqS))

(define seqrem
  (lambda (new old l)
    l))

(define yyy
  (lambda (a l)
    ((insert-g seqrem) #f a l)))

;(define a '(sausage))
;(define l '(pizza with sausage and bacon))
;(yyy a l)
; (define l '(shrimp salad tuna salad and tuna))
; (define a 'tuna)
; (define test? eq?)
; ((multirember-f test?) a l)
; (define multirember-eq? (multirember-f test?))

(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond ((null? lat) '())
            ((test? a (car lat)) 
             ((multirember-f test?) a (cdr lat)))
            (else (cons (car lat)
                        ((multirember-f test?) a (cdr lat))))))))

(define multiremberT
  (lambda (test? lat)
    (cond ((null? lat) '())
          ((test? (car lat))
           (multiremberT test? (cdr lat)))
          (else (cons (car lat)
                      (multiremberT test?
                                    (cdr lat)))))))

(define a-friend
  (lambda (x y)
    (null? y)))

(define multirember-co
  (lambda (a lat col)
    (cond ((null? lat)
           (col '() '()))
          ((eq? (car lat) a)
           (cdr lat)
           (lambda (newlat seen)
             (col newlat
                  (cons (car lat) seen))))
          (else 
           (multirember-co a
                           (cdr lat)
                           (lambda (newlat seen)
                             (col newlat
                                  (cons (car lat) seen))))))))

(define new-friend
  (lambda (newlat seen)
    (col newlat
         (cons (car lat) seen))))

(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond ((null? lat) '())
          ((eq? (car lat) oldL)
           (cons new 
                 (cons oldL 
                       (multiinsertLR new oldL oldR
                                      (cdr lat)))))
          ((eq? (car lat) oldR)
           (cons oldR
                 (cons new
                       (multiinsertLR new oldL oldR
                                      (cdr lat)))))
          (else (cons (car lat)
                      (multiinsertLR new oldL oldR
                                     (cdr lat)))))))

(define multiinsertLR-co
  (lambda (new oldL oldR lat col)
    (cond ((null? lat)
           (col '() 0 0))
          ((eq? (car lat) oldL)
           (multiinsertLR-co new oldL oldR
                             (cdr lat)
                             (lambda (newlat L R)
                               (col (cons new
                                          (cons oldL newlat))
                                    (add1 L) R))))
          ((eq? (car lat) oldR)
           (multiinsertLR-co new oldL oldR
                             (cdr lat)
                             (lambda (newlat L R)
                               (col (cons oldR
                                          (cons new newlat))
                                    L (add1 R)))))
          (else 
           (multiinsertLR-co new oldL oldR
                             (cdr lat)
                             (lambda (newlat L R)
                               (col (cons (car lat) newlat)
                                    L R)))))))

(define evens-only*
  (lambda (l)
    (cond ((null? l) '())
          ((atom? (car l))
           (cond ((even? (car l))
                  (cons (car l)
                        (evens-only* (cdr l))))
           (else (evens-only* (cdr l)))))
          (else (cons (evens-only* (car l))
                      (evens-only* (cdr l)))))))

(define keep-looking
  (lambda (a sorn lat)
    (cond
      ((number? sorn)
       (keep-looking a (pick sorn lat) lat))
      (else (eq? sorn a)))))

(define shift
  (lambda (pair)
    (build (first (first pair))
           (build (second (first pair))
                  (second pair)))))

(define align
  (lambda (pora)
    (cond ((atom? pora) pora)
          ((a-pair? (first pora))
           (align (shift pora)))
          (else (build (first pora)
                       (align (second pora)))))))

(define length*
  (lambda (pora)
    (cond ((atom? pora) 1)
          (else (+ (length* (first pora))
                   (length* (second pora)))))))

(define shuffle
  (lambda (pora)
    (cond ((atom? pora) pora)
          ((a-pair? (first pora))
           (shuffle (revpair pora)))
          (else (build (first pora)
                       (shuffle (second pora)))))))

(define C
  (lambda (n)
    (cond ((one? n) 1)
          (else (cond ((even? n) (C (div n 2)))
                      (else (C (add1 (mul 3 n)))))))))

(define A
  (lambda (n m)
    (cond ((zero? n) (add1 m))
          ((zero? m) (A (sub1 n) 1))
          (else (A (sub1 n)
                   (A n (sub1 m)))))))
