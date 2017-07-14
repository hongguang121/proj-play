#lang racket
(require xml)

;XML解析器

(define xml (file->string "f:/emotion_config.xml"))

;格式整理，去掉所有换行符，保留空格符


;字符串str是否以sym开头
(define (start-with? str sym)
  (define (iter start end)
    (if (string=? (substring str start end) sym)
        #t
        #f))
  (iter 0 (string-length sym)))

;字符串str是否以sym结尾
(define (end-with? str sym)
  (define (iter len end)
    (if (string=? (substring str (- len end)) sym)
        #t
        #f))
  (iter (string-length str) (string-length sym)))

;过滤xml文件头
(define (filter-title str)
  (define (iter start end)
    (if (and (start-with? str start) (end-with? str end))
        str
        #f))
  (iter "<?" "?>"))

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

