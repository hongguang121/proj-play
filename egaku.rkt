#lang slideshow
(require pict/flash)
(require racket/class
         racket/gui/base)
(require slideshow/code)

(define (four shape i)    
      (let ((line (hc-append i shape shape)))
        (vc-append i line line)))

(define (square n . color)
  (if (null? color)
      (filled-rectangle n n)
      (colorize (filled-rectangle n n) (car color))))

(define (series mk)
    (hc-append 4 (mk 5) (mk 10) (mk 20)))

(define (checker p1 p2)
  (let ([p12 (hc-append p1 p2)]
        [p21 (hc-append p2 p1)])
    (vc-append p12 p21)))

(checker (colorize (square 10) "red")
         (colorize (square 10) "black"))

(define (checkerboard p)
  (let* ([rp (colorize p "red")]
         [bp (colorize p "black")]
         [c (checker rp bp)]
         [c4 (four c 0)])
    (four c4 0)))

(define (rainbow p)
  (map (λ (color) (colorize p color))
       (list "red" "orange" "yellow" "green" "blue" "purple")))

(apply vc-append (rainbow (square 10)))

(define-syntax pict+code
  (syntax-rules ()
    [(pict+code expr)
     (hc-append 10
                expr
                (code expr))]))

(define f (new frame% [label "My Art"]
                      [width 300]
                      [height 300]
                      [alignment '(center center)]))

(define (add-drawing p)
  (let ([drawer (make-pict-drawer p)])
    (new canvas% [parent f]
                 [style '(border)]
                 [paint-callback (lambda (self dc)
                                   (drawer dc 0 0))])))

(add-drawing (pict+code (circle 10)))
(add-drawing (colorize (filled-flash 50 30) "yellow"))
(add-drawing (colorize (square 50) "red"))

(send f show #t)

