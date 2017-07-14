#lang racket

(define (unless exp exception norm)
  (if exp norm exception))
