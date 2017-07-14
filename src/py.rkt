#lang racket

(define filepath-1 "I:/node-project/pinyin.txt")
(define filepath-2 "I:/node-project/zimu.txt")

(define py (file->string filepath-1))
(define zm (file->string filepath-2))

(define (parse str)
  (define (iter ls rs)
    (cond ((null? ls) '())
          ((eq? #\< (car ls))
           (iter (cdr ls) rs))
          ((eq? #\> (car ls))
           (list->string (reverse rs)))
          (else (iter (cdr ls)
                      (cons (car ls) rs)))))
  (iter (string->list str) '()))

(define (to-string dict cnt)
  (list->string (list (string-ref dict cnt))))

;全部是string
(define (exist? word dict)
  (define (iter cnt)
    (cond ((= (string-length dict) cnt) #f)
          ((string=? (to-string dict cnt) word) #t)
          (else (iter (+ cnt 1)))))
  (iter 1))

(define (next-py? next pdict zdict)
    (cond ((= (string-length pdict) next) #f)
          (else (cond ((exist? (to-string pdict next) zdict) #t)
                       (else #f)))))

(define (find-py word)
  (define (iter pdict zdict cnt remb)
    (cond ((= (string-length py) cnt) '())
          ((string=? (to-string pdict cnt) word) remb)
          (else (cond ((exist? (to-string pdict cnt) zdict)
                       (iter pdict
                             zdict
                             (+ cnt 1)
                             (string-append remb (to-string pdict cnt))))
                      (else (cond ((next-py? (+ cnt 1) pdict zdict)
                                   (iter pdict zdict (+ cnt 1) ""))
                                  (else (iter pdict zdict (+ cnt 1) remb))))))))
  (iter py zm 1 ""))

(define (find-str str)
  (define (iter cnt)
    (cond ((= (string-length str) cnt) "")
          (else (string-append (find-py (to-string str cnt))
                               " "                     
                               (iter (+ cnt 1))))))
  (iter 0))