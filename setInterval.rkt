#lang racket

;避免传递函数参数时被求值，可以通过(lambda () ...)和(eval '(expr))等形式，总之，参数必须作为引用传递。

(define (Interval callback interval)
  (thread (λ ()
            (let loop ()
              (callback)
              (sleep interval)
              (loop)))))

(define-syntax (setInterval stx)
  (datum->syntax stx `(Interval ,(cadr (syntax->datum stx)) ,(caddr (syntax->datum stx)))))

(define-syntax (setInterval+ stx)
  (syntax-case stx ()
    [(_ callback interval)
     #'(Interval callback interval)]))

(define (say name) (displayln name (current-output-port)))

;(setInterval (λ () (say 'hi)) 1)

(provide Interval setInterval)

