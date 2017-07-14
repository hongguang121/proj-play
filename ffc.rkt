#lang racket

(require racket/cmdline)
(require racket/date)
(require racket/struct)
(require racket/draw)

(define records (file->list "e:/source/ffc/records.txt"))

(define id 0)

(define (set-id)
  (if (null? records)
      (set! id 0)
      (set! id (+ (car (last (file->list "e:/source/ffc/records.txt"))) 1))))

(set-id)

;重新整理id
(define (arr-id) 
  (letrec ((A (λ (lr id rem)
              (cond ((null? lr)
                     (display-lines-to-file (reverse rem) "e:/source/ffc/records.txt" #:mode 'text #:exists 'replace)
                     (set! records (file->list "e:/source/ffc/records.txt")))
                    (else (A (cdr lr) (+ id 1) (cons (cons id (cdar lr)) rem)))))))
    (A records 0 '())))
    

;当前系统时间格式化：日期 小时 分钟 秒
(define (format-date)
  (let ((cd (struct->list (current-date))))
    (format "~a-~a-~a ~a:~a:~a"
            (list-ref cd 5)
            (list-ref cd 4)
            (list-ref cd 3)
            (list-ref cd 2)
            (list-ref cd 1)
            (list-ref cd 0))))

;加入记录及精确时间
(define (add interval)
  (let ((record (list (cons id (cons (format-date) (list interval))))))    
      (display-lines-to-file record "e:/source/ffc/records.txt" #:mode 'text #:exists 'append)
      (set! records (file->list "e:/source/ffc/records.txt"))
      (set! id (+ id 1))))

;修改记录 new = '(0 2016-10-24 10:49:13 1)
(define (change-iter new)
  (let ((lr records)
        (idn (car new)))
    (letrec ((C (λ (lr)
                  (cond ((null? lr) '())
                        ((= (caar lr) idn)
                         (cons new (C (cdr lr))))
                        (else (cons (car lr) (C (cdr lr))))))))
      (C lr))))

;修改*
(define (change new)
  (let ((rds (change-iter new)))
    (display-lines-to-file rds "e:/source/ffc/records.txt" #:mode 'text #:exists 'replace)
    (set! records (file->list "e:/source/ffc/records.txt"))))

;删除记录
;注意删除的idn和全局变量id不可同名
(define (rev-iter idn)
  (let ((lr records))
    (letrec ((R (λ (lr)
                  (cond ((null? lr) '())
                        ((= (caar lr) idn) (R (cdr lr)))
                        (else (cons (car lr) (R (cdr lr))))))))
      (R lr))))

;删除*
(define (rev idn)
  (let ((rds (rev-iter idn)))
    (display-lines-to-file rds "e:/source/ffc/records.txt" #:mode 'text #:exists 'replace)
    (set! records (file->list "e:/source/ffc/records.txt"))))

;命令行操作
(define rds (λ () (display records)))

(define ffc
  (command-line
   #:program "ffc"
   #:once-each
   [("-r" "--rds") "show records" (rds)]
   [("-a" "--add") arg "add a record" (add arg)]
   [("-v" "--rev") arg "remove a record" (rev arg)]))


