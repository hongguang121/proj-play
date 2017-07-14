#lang racket

(require web-server/servlet
         web-server/servlet-env
         web-server/formlets
         web-server/formlets/syntax)

(require json)

(provide/contract (start (request? . -> . response?)))

(define req-info '())
(define content '())
(define j (file->string "e:/test.json"))
(define k (substring j 1))
(define l (string->jsexpr k))

(define (start request)
  ;(set! req-info (request-post-data/raw request))
  ;(set! req-info (extract-binding/single 'title (request-bindings request)))
  (set! req-info (request-bindings request))
  (set! content req-info)
  (render-response-page req-info request))

(define (render-response-page req-info request)
  (response
   200
   #"Moved Permanently"
   (current-seconds)
   TEXT/HTML-MIME-TYPE
   (list (make-header #"Access-Control-Allow-Origin"
                      #"*"))
   (λ (op) (write-string (cdr (list-ref req-info 0)) op))))

(serve/servlet start
               #:launch-browser? #f
               #:quit? #f
               #:listen-ip #f
               #:port 8400
               #:extra-files-paths (list "e:\\hg\\MyWordsWeb")
               #:servlet-path "/sendback"
               #:command-line? #f
               #:stateless? #f)