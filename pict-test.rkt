#lang racket
(require pict)

(define (do-fade n)
    (fade-pict n (rectangle 30 30) (disk 30)))

(apply ht-append 10
         (for/list ([n (in-range 0 1.2 0.2)])
           (vc-append (text (~r n #:precision 2))
                      (do-fade n))))

#|
(define (do-slide n)
    (define p1 (disk 30 #:color "plum"))
    (define p2 (disk 30 #:color "palegreen"))
    (define p3 (frame (inset (hc-append 30 p1 p2) 10)))
    (slide-pict p3
                (disk 10)
                p1 p2 n))

(apply ht-append 10
         (for/list ([n (in-range 0 1.2 0.2)])
           (vc-append (text (~r n #:precision 2))
                      (do-slide n))))
|#

(define (do-slide n)
    (define p1 (disk 30 #:color "plum"))
    (define p2 (disk 30 #:color "palegreen"))
    (define p3 (frame (inset (hc-append 30 p1 p2) 10)))
    (slide-pict/center p3
                       (disk 10)
                       p1 p2 n))

(apply ht-append 10
         (for/list ([n (in-range 0 1.2 0.2)])
           (vc-append (text (~r n #:precision 2))
                      (do-slide n))))