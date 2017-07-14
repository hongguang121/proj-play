#lang racket

(require 2htdp/image
         racket/cmdline
         (only-in racket/draw read-bitmap))
;(require pict)
(require 2htdp/universe)

(define rocket (read-bitmap "k:/rocket.png"))
(define rotation (read-bitmap "e:/rotation.png"))
(define arrow (read-bitmap "e:/arrow.png"))

;(set! nf (scale/xy 0.31 0.25 (crop 100 0 800 1200 nf)))
(set! rotation (scale/xy 0.5 0.5 rotation))
(set! arrow (scale/xy 0.5 0.5 arrow))

(define (img-data img)
  (let ((width (image-width img))
        (height (image-height img)))
    (list width height)))

(define eps (empty-scene 100 60))

(define (picture-of-rocket height)
  (place-image rocket 50 height eps))


(define base "G:\\Youku Files\\pictures")
(define blst (directory-list base))

(define (read-pic)
  (map (λ (path) (scale/xy 0.25 0.25 (read-bitmap path)))
       (map (λ (x) (build-path base x)) blst)))

(define (kusi ang)
  (overlay arrow (rotate ang rotation)))

(define (ten? x) (equal? x 10))

(define turn-around
  (thread (λ ()
 (animate (lambda (x)
           (place-image (kusi x) 160 160 (empty-scene 320 320)))))))

