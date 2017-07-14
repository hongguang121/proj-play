#lang racket

;hanoi
(define display-tower-hanoi
  (let ((show-move (λ (n s d)
                     (display n)
                     (display " = ")
                     (display s)
                     (display " --> ")
                     (display d))))
    (λ (n)
      (letrec
          ((move
            (λ (n source destination helper)
              (if (= n 1)
                  (begin
                    (show-move n source destination)
                    (newline))
                  (begin
                    (move (- n 1) source helper destination)
                    (show-move n source destination)
                    (display ", ")
                    (move (- n 1) helper destination source))))))
        (move n 'L 'R 'C)))))

;eight-queens
;生成一张8*8的矩阵
(define matrix (make-list 8 (make-list 8 0)))


