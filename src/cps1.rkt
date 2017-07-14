(define cps
  (lambda (exp)
    (letrec
        ([trivs '(zero? add1 sub1)]
         [id (lambda (v) v)]
         [C~ (lambda (v) `(k ,v))]
         [fv (let ((n -1))
               (lambda ()
                 (set! n (+ 1 n))
                 (string->symbol (string-append "v" (number->string n)))))]
         [cps1
          (lambda (exp C)
            (pmatch exp
              [,x (guard (not (pair? x))) (C x)]
              [(lambda (,x) ,body)
               (C `(lambda (,x k) ,(cps1 body C~)))]
              [(,rator ,rand)
               (cps1 rator
                     (lambda (r)
                       (cps1 rand
                             (lambda (d)
                               (cond
                                [(memq r trivs)
                                 (C `(,r ,d))]
                                [(eq? C C~)         ; tail call
                                 `(,r ,d k)]
                                [else
                                 (let ([v* (fv)])
                                   `(,r ,d (lambda (,v*) ,(C v*))))])))))]))])
      (cps1 exp id))))