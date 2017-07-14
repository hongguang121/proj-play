#lang racket
(require racket/trait)

(define fish-interface (interface () get-size grow eat))

(define fish%
  (class* object%
    (fish-interface)
    (init size)                ; initialization argument 
    (define current-size size) ; field
    (define name　"uo")
    (super-new)                ; superclass initialization

    (define/public (get-name)
      name)

    (define/public (set-name _name)
      (set! name _name))
    
    (define/public (get-size)
      current-size)
 
    (define/public (grow amt)
      (set! current-size (+ amt current-size)))
 
    (define/public (eat other-fish)
      (grow (send other-fish get-size)))))

(define charlie (new fish% [size 10]))

(define get-fish-size (generic fish% get-size))

(send-generic charlie get-fish-size)

(define hungry-fish% (class fish% (super-new)
                       (inherit eat)
                       (define/public (eat-more fish1 fish2)
                         (eat fish1) (eat fish2))))

(define picky-fish% (class fish% (super-new)
                      (define/override (grow amt) 
                        (super grow (* 3/4 amt)))))

(define daisy (new picky-fish% [size 20]))

(send-generic daisy get-fish-size)

(define size-10-fish% (class fish% (super-new [size 10])))

(send (new size-10-fish%) get-size)

(define default-10-fish% (class fish%
                           (init [size 10])
                           (super-new [size size])))

(define richard (new default-10-fish% [size 20]))

(define (picky-mixin %)
  (class % (super-new)
    (define/override (grow amt) (super grow (* 3/4 amt)))))
(define picky+-fish% (picky-mixin fish%))

(define (hungry-mixin %)
  (class % (super-new)
    (inherit eat)
    (define/public (eat-more fish1 fish2)
      (eat fish1)
      (eat fish2))))
(define hungry+-fish% (hungry-mixin fish%))

(define picky-hungry-fish%
  (hungry-mixin (picky-mixin fish%)))

(define person%
  (class object%
    (init name age)
    (super-new)
    (define current-name name)
    (define current-age age)
    (define/public (get-name) current-name)
    (define/public (get-age) current-age)
    (define/public (eat food) (printf "~a eated ~a." current-name food))
    (define/public (grow amt) (set! current-age (+ current-age amt)))))
(define child% (hungry-mixin (picky-mixin person%)))
(define oliver (new child% [name "Oliver"] [age 6]))

(define choosy-interface (interface () choose?))
(define hungry-interface (interface () eat))
(define choosy-eater-mixin
    (mixin (choosy-interface) (hungry-interface)
      (inherit choose?)
      (super-new)
      (define/public (eat x)
        (cond
          [(choose? x)
           (printf "chomp chomp chomp on ~a.\n" x)]
          [else
           (printf "I'm not crazy about ~a.\n" x)]))))

(define herring-lover%
    (class* object% (choosy-interface)
      (super-new)
      (define/public (choose? x)
        (regexp-match #px"^herring" x))))

(define herring-eater% (choosy-eater-mixin herring-lover%))
(define eater (new herring-eater%))
(send eater eat "elderberry")
(send eater eat "herring")
(send eater eat "herring ice cream")

(define (make-hungry-mixin eat-method-key)
  (define-member-name eat eat-method-key)
  (mixin () () (super-new)
    (inherit eat)
    (define/public (eat-more x y) (eat x) (eat y))))

(define spots-trait
  (list (cons (member-name-key get-color)
              (lambda (get-color-key %)
                (define-member-name get-color get-color-key)
                (class % (super-new)
                  (define/public (get-color) 'black))))))

(define stripes-trait
  (trait
   (define/public (get-color) 'red)))

(define animal%
  (class object%
    (super-new)
    (field [size 10])
    (define/public (eat food)
      (set! size (+ size (get-field size food))))))

(define car%
  (class object%
    (super-new)
    (field [name ""]
           [factory ""]
           [price ""])
    (define/public (set-name arg)
      (set! name arg))
    (define/public (get-name)
      name)))

