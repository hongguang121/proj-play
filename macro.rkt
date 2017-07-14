#lang racket
(require (for-syntax racket/match))

(define-syntax-rule (swap x y)
  (let ([tmp x])
    (set! x y)
    (set! y tmp)))

(define-syntax foo
  (λ (stx)
    (syntax "I am foo")))

(define-syntax (also-foo stx)
    ;(syntax "I am also foo"))
  (syntax stx))

(define-syntax (quoted-foo stx)
    #'"I am also foo, using #' instead of syntax")

(define-syntax (say-hi stx)
    #'(displayln "hi"))

(define-syntax (show-me stx)
    (print stx)
    #'(void))

(show-me '(+ 1 2))

(define-syntax (reverse-me stx)
    (datum->syntax stx (reverse (cdr (syntax->datum stx)))))

(reverse-me "backwards" "am" "i" values)

(define (our-if condition true-expr false-expr)
    (cond [condition true-expr]
          [else false-expr]))

(define (display-and-return x)
    (displayln x)
    x)

(our-if #t
          "true"
          "false")

(our-if #t
          (display-and-return "true") ;表达式在作为参数传递时已经被求值了
          (display-and-return "false"))

(define-syntax (our-if-v2 stx)
    (define xs (syntax->list stx))
    (datum->syntax stx `(cond [,(cadr xs) ,(caddr xs)]
                              [else ,(cadddr xs)])))

(our-if-v2 #t
           (display-and-return "true")
           (display-and-return "false"))

(define-syntax (our-if-using-match stx)
    (match (syntax->list stx)
      [(list name condition true-expr false-expr)
       (datum->syntax stx `(cond [,condition ,true-expr]
                                 [else ,false-expr]))]))

(define-syntax (our-if-using-match-v2 stx)
    (match (syntax->list stx)
      [(list _ condition true-expr false-expr)
       (datum->syntax stx `(cond [,condition ,true-expr]
                                 [else ,false-expr]))]))

(our-if-using-match-v2 #t "true" "false")
