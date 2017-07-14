#lang racket

(require json)
(require data/queue)

(define j (file->string "e:/list.json"))
(define k (substring j 1))
(define l (string->jsexpr k))

(define q (make-queue))

(enqueue! q 1)
(dequeue! q)
(enqueue-front! q 2)
(enqueue-front! q 3)
(queue-filter! q even?)
(queue->list q)
(queue-length q)

;data: json list (request contents)
(define (data->json data)
  (let ((key (map car data))
        (val (map cdr data)))
    (string-append "{"
                   (map (λ (k v) (string-append (symbol->string k) ":" (symbol->string v) ",")) key val)
                   "}")))