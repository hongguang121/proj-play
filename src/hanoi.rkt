(define (hanoi n)
  (define (iter disc src aux dst)
    (if (> disc 0)
        (begin
          (iter (- disc 1) src dst aux)
          (format "Move disc ~s from ~s to ~s" disc src dst)
          (iter (- disc 1) aux src dst))))
  (iter n 'Src 'Aux 'Dst))
