#lang racket

(require net/http-client)
(require net/uri-codec)
(require json)

(define host "localhost")
;(define port 8100)
(define port 3000)

(define (hc host port)
  (http-conn-open host #:port port))

(define-values (p1 p2 in)
  (http-conn-sendrecv!
   (hc host port) "/login"
   #:method "POST"
   #:data
   (alist->form-urlencoded
    (list (cons 'username "Ryu")
          (cons 'password "Sheng Long")))
 
   #:headers (list "Content-Type: application/x-www-form-urlencoded")))

;result
;(read in)