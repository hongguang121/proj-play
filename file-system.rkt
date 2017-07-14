#lang racket

;异步文件加载
(define (read-file file-path callback)
  (thread
   (λ ()
     (with-handlers ([exn:fail?
                      (λ (exn)
                        (callback exn #f))])
       (callback #f (file->string file-path))))))

#|
(read-file "e:/test.txt"
           (λ (err data)
             (if err
                 (displayln err)
                 (displayln data))))
|#

;watch file
;查找和orgin不同之处
;origin/curr = (file->lines *)
(define (check-edit origin current)
  (let loop ((org origin)
             (cur current)
             (res '())) ;no edit
    (cond ((and (null? org) (null? cur)) (reverse res))
          ((null? cur) (reverse (cons org res)))
          ((null? org) (reverse (cons cur res)))
          ((not (= (string-length (car org)) (string-length (car cur))))
           (loop (cdr org) (cdr cur) (cons cur res)))
          (else (loop (cdr org) (cdr cur) res)))))

(define (watch-file file-path callback)
  (thread
   (λ ()
     (let ((edit-time (file-or-directory-modify-seconds file-path))
           (origin (file->lines file-path)))
       (let loop ()
         (let ((curr-time (file-or-directory-modify-seconds file-path)))
           (when (> curr-time edit-time)
             (set! edit-time curr-time)
             (let ((curr (file->lines file-path)))
               (unless (null? (check-edit origin curr)) ;edited
                 (callback (check-edit origin curr))
                 (set! origin curr)                
                 ))))
         (sleep 1)
         (loop)
         )))))

;generate log-file
(define (gen-log log)
  (display-to-file (format "log: ~a" log) "e:/log.txt" #:exists 'append))