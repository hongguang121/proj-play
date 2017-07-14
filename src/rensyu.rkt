#lang racket

(define (qsort ls)
  (if (null? ls)
      '()
      (let ((n (list-ref ls (round (/ (length ls) 2)))))
        (append
         (qsort (filter (λ (x) (< x n)) ls))
         (filter (λ (x) (= x n)) ls)
         (qsort (filter (λ (x) (> x n)) ls))))))

(define (gen-num m n)
  (for/list ((i n))
    (random m)))

(define (bsearch ls n)
  (let* ((id (round (/ (length ls) 2)))
         (mid (list-ref ls id)))
    (cond ((= (length ls) 1) '())
          ((< n mid)
           (bsearch (take ls id) n))
          ((> n mid)
           (bsearch (drop ls id) n))
          (else n))))

(define ls (qsort (gen-num 100 10)))

;;event

(define ch1 (make-channel))
(define ch2 (make-channel))

(define either-channel (choice-evt ch1 ch2))
(thread (λ () (displayln (sync either-channel))))

;(define th1 (thread (λ () (displayln (sync ch1)) (display (sync ch2)))))

;(channel-put ch1 'Hello)
;(channel-put ch2 'World)

(if (> (random) 0.5)
    (channel-put ch1 'tuturuu)
    (channel-put ch2 'turuturu))

(define ch3 (make-channel))
(define evt (wrap-evt ch3 (λ (v) (format "you've got mail: ~a" v))))
(thread (λ () (displayln (sync evt))))
(channel-put ch3 "Dear Alice ...")

(define msg-ch (make-channel))
(define exit-ch (make-channel))

(thread
   (λ ()
     (let loop ([val 0])
       (printf "val = ~a~n" val)
       (sync (handle-evt
              msg-ch
              (λ (val) (loop val)))
             (handle-evt
              exit-ch
              (λ (val) (displayln val)))))))

(channel-put msg-ch 5)
(channel-put exit-ch 'done)


