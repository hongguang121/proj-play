(load "j-bob-lang.scm")
(load "j-bob.scm")
(load "little-prover.scm")
(dethm.align/align)

;chapter 3

(defun pair (x y)
  (cons x (cons y '())))

(defun first-of (x)
  (car x))

(defun second-of (x)
  (car (cdr x)))

(defun in-pair? (xs)
  (if (equal (first-of xs) '?)
      't
      (equal (second-of xs) '?)))

;chapter 4

(defun list0? (x)
  (equal x '()))

(defun list1? (x)
  (if (atom x)
      'nil
      (list0? (cdr x))))

(defun list2? (x)
  (if (atom x)
      (list1? (cdr x))))

(defun list? (x)
  (if (atom x)
      (equal x '())
      (list? (cdr x))))

(cons (cons 1 (cons (cons 'a (cons 2 '())) '())) (cons'b '()))





