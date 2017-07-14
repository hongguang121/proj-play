#lang racket

(define find-lst
  (λ (lst index)
    (if (= index 0)
        '()
        (cons (car lst)
              (find-lst (cdr lst) (- index 1))))))

(define find
  (λ (lst index)
    (cond ((null? lst) '())
          ((= index 0) (car lst))
          (else (find (cdr lst) (- index 1))))))

(define splice
  (λ (lst index flag value)
    (cond ((null? lst) '())
          ((= index 0)
           (cond ((= flag 0) (cons value lst))
                 (else (splice (cdr lst) index (- flag 1) value))))
          (else (cons (car lst)
                      (splice (cdr lst) (- index 1) flag value))))))

(splice '(George John Thomas James Adrew Martin) 2 0 'William)
(splice '(George John Thomas James Adrew Martin) 2 1 'William)
(splice '(George John Thomas James Adrew Martin) 2 3 'William)

(define node
  (λ (lst)
    (letrec ((R (λ (root left right index mem rt)
                  (cond ((null? (find lst index)) (cons rt (cons (list left) (list right))))
                        ((null? root) (R (find lst index) left right (+ index 1) mem (find lst index)))
                        ((< (find lst index) root)
                         (cond ((null? left)
                                (R root (reverse (cons (find lst index) mem)) right (+ index 1) left rt))
                               (else (R (car left) (cdr left) right index left rt))))
                        ((> (find lst index) root)
                         (cond ((null? right)
                                (R root left (reverse (cons (find lst index) mem)) (+ index 1) right rt))
                               (else (R (car right) left (cdr right) index right rt))))))))
             (R '() '() '() 0 '() '()))))

