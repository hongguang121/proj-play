#lang racket

(define result '())

;(thread-send sirase (λ (x) (display x)))
(define sirase
  (thread (λ ()
            (let* ((rec (thread-receive))
                   (callback (if (null? rec)
                                 (λ (res) (printf "~a\n" res))
                                 rec)))
              (let loop ()
                (if (null? result)
                    (begin
                      (sleep 5)
                      (loop))
                    (begin
                      ;(printf "~a~\n" result)
                      (apply callback (list result))
                      (newline)
                      (set! result '())
                      (loop))))))))
;(read-file "e:/test.txt" (λ (x) x))
;(read-file "e:/test.txt" file->list (λ (x) (display x)))
(define (read-file . args)
  (thread (λ ()
  (let ((path (first args))        
        (callback (last args)))
    (if (= (length args) 2)
        (let ()
          (set! result
                ;(with-handlers ([exn:fail:filesystem:exists?
                ;                 (λ (exn) 'error)])
                (file->string path))
          (thread-send sirase callback))
        (let ((method (if (eq? null? (second args))
                          (file->string)
                          (second args))))
          (set! result (method path))
          (thread-send sirase callback)))))))

(read-file "e:/test.txt" file->list (λ (x) (display x)))
(display "测试异步读取文件\n")