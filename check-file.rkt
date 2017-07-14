#lang racket

(define *file-path* "e:/test.txt")
(define *modify-seconds* (file-or-directory-modify-seconds *file-path*))
(define *main-thread* (current-thread))
(define *mem* "")

(define (load-file file-path)  
  (set! *mem* (file->lines file-path)))
;init load-file
(load-file *file-path*)

;(check-diff (file->lines file-path))
;return: (list mem curr)
(define (check-diff file)
  (let loop ((mem *mem*)
             (curr file)           
             (res '()))
    (cond ((and (null? mem) (null? curr)) (reverse res))
          ((null? curr) (reverse (cons (append mem '("null")) res)))
          ((null? mem) (reverse (cons (append '("null") curr) res)))
          ((not (= (string-length (car mem)) (string-length (car curr))))
           (loop (cdr mem) (cdr curr) (cons (list (car mem) (car curr)) res)))
          (else (loop (cdr mem) (cdr curr) res)))))

(define (change? file-path)
  (thread
   (λ ()     
     (let loop ()
       (let ((curr (file-or-directory-modify-seconds file-path))
             (res '()))
         (when (> curr *modify-seconds*)
           (let ()
             (set! *modify-seconds* curr)            
             (set! res (check-diff (file->lines file-path)))
             (load-file file-path) ;update file memory
             (unless (null? res)
               ;(thread-send *main-thread* (format "~a" true))))              
               (thread-send listener (list #t res))))
           ;(thread-send *main-thread* (format "~a" false))
           ))
       (sleep 1)
       (loop)))))

(define listener
  (thread
   (λ ()
     (let loop ((rev (thread-receive)))
       (displayln rev)
       (when (car rev)
         (map
          (λ (x)
            (displayln (format "file has changed from ~a to ~a" (car x) (cadr x))))
          (cadr rev)))
       (sleep 1)
       (loop (thread-receive))))))

;(thread-receive)

(change? *file-path*)

(provide (all-defined-out))