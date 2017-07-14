#lang racket

(require racket/date)
(require file/sha1)
(require file/md5)

(define path-set '())

;date = (current-date)
(define (ymd date)
  (list (number->string (date-year date))
        (number->string (date-month date))
        (number->string (date-day date))))

;date = (current-date)
(define (format-date date)
  (let ((year (number->string (date-year date)))
        (month (number->string (date-month date)))
        (day (number->string (date-day date)))
        (hour (number->string (date-hour date)))
        (minute (number->string (date-minute date)))
        (second (number->string (date-second date))))
    (string-append year "-" month "-" day " " hour ":" minute ":" second)))

;pdate = (ymd date)
(define (generator-path pdate)
  (let loop ((curr "e:/pics")
             (lst pdate))       
    (if (not (null? lst))
        (begin
          ;(displayln lst)
          (if (directory-exists? curr)              
              (loop (string-append curr "/" (car lst)) (cdr lst))
              (begin
                (make-directory curr)
                (loop (string-append curr "/" (car lst)) (cdr lst)))))
        (begin
          (if (directory-exists? curr)
              (begin 
                "Path already existed!"
                (string-append curr "/"))
              (begin
                (make-directory curr)
                (set! path-set (cons (string-append curr "") path-set))))))))

;生成路径文件夹
(generator-path (ymd (current-date)))

;随机生成名称
(define (generator-name)
  (let ((rand-name (number->string (random 1 4294967087))))
    (sha1 (open-input-string rand-name))))

;拼接地址
;type: 'jpg/'mp4 curr:(generator-path (ymd (current-date)))
(define (gen type curr)
  (match type
    ('jpg (string-append curr (generator-name) ".jpg"))
    ('mp4 (string-append curr (generator-name) ".mp4"))
    ('txt (string-append curr (generator-name) ".txt"))
    (_ (string-append curr (generator-name)))))

(provide (all-defined-out))