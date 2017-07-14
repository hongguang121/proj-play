#lang racket
(require json)
(require openssl/md5)
(require openssl/sha1)

(define in (open-input-file "e:/hg/json.txt"))


(define j (file->string "e:/hg/json.txt"))
(define k (file->string "e:/good.txt"))

(string->jsexpr j)

(define jb (bytes->jsexpr (file->bytes "e:/hg/json.txt")))

(hash-ref (list-ref jb 0) 'City)

#hash(("apple" . red)
      ("banana" . yellow))

#|
(define read-all
  (let loop ((curr 0)
             (res ""))
    (if (eof-object? in)
        res
        (loop curr (string-append res (read-string curr in))))))
|#

(gensym)

(gensym "Apple")

(printf "md5: ~a\n" (md5 in))
(printf "sha1: ~a\n"(sha1 in))

(close-input-port in)

;事件必须是一个thread

(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1))
         (fib (- n 2)))))

(define (thunk)
    (thread (λ ()
              (sleep 1)
              (displayln "Hi! I'm a thunk"))))

(evt? never-evt)

(evt? (thunk))

(sync (thunk))

(sync/timeout 0.5 (thunk)) ;0.5s后观察事件，没有则返回#f，即只能观察到该时限内的事件

(sync/timeout 1 (thread (λ () (fib 38))))

(define ch1 (make-channel))

(define ch2 (make-channel))

(define (loop)
  (thread (λ () (displayln (sync ch1 ch2)) (loop))))

;(channel-put ch1 (fib 10))


