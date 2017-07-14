#lang racket
(define quick-sort
 (λ (s)
  (if (< (length s) 2)
   s
   (append
    (quick-sort
	(filter
          (λ (e)
	    (< e (last s)))
          s))
    (filter
      (λ (e)
        (= e (last s)))
      s)
    (quick-sort
	(filter
          (λ (e)
	    (> e (last s)))
          s))))))