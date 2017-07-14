#lang racket

(define 1st car)
(define 2nd cadr)

(define for-effect-only
  (λ (item-ignored)
    "unspecified value"))

(define box-maker
  (λ (init-value)
    (let ((contents init-value))
      (λ msg
        (case (1st msg)
          ((type) "box")
          ((show) contents)
          ((update!) (for-effect-only (set! contents (2nd msg))))
          ((swap!) (let ((ans contents))
                     (set! contents (2nd msg))
                     ans))
          ((reset!) (for-effect-only (set! contents init-value)))
          (else (delegate base-object msg)))))))

(define delegate
  (λ (obj msg)
    (apply obj msg)))

(define base-object
  (λ msg
    (case (1st msg)
      ((type) "base-object")
      (else invalid-method-name-indicator))))

(define invalid-method-name-indicator "unknown")

(define send
  (λ args
    (let ((object (car args))
          (message (cdr args)))
      (let ((try (apply object message)))
        (if (eq? invalid-method-name-indicator try)
            (error "Bad method name:" (car message)
                   "sent to object of"
                   (object 'type)
                   "type.")
            try)))))

(define counter-maker
  (λ (init-value unary-proc)
    (let ((total (box-maker init-value)))
      (λ msg
        (case (1st msg)
          ((type) "counter")
          ((update!) (let ((result (unary-proc (send total 'show))))
                       (send total 'update! result)))
          ((swap!) (delegate base-object msg))
          (else (delegate total msg)))))))


