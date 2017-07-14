#lang racket

;已经知道原价和税率，求含税价
(define (komi genka ritu)
  (+ genka (* genka ritu)))

;已经知道含税价和税率，求原价
(define (nuki zeikin ritu)
  (/ zeikin (+ 1 ritu)))

