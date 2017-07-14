#lang racket

(require web-server/servlet
         web-server/servlet-env
         web-server/formlets
         web-server/formlets/syntax)

(require net/url)
(require json)
(require racket/date)

(define req-info '())

(define (start request)
  (set! req-info (request-bindings request))
  (render-response-page req-info request))

(define (render-response-page contents request)
  (response
   200 #"Moved Permanently"
   (current-seconds) ;TEXT/HTML-MIME-TYPE
   #"text/plain; charset=utf-8"
   (list (make-header
          #"Access-Control-Allow-Origin"
          #"*"
          ))
   (λ (op) (write-string "{\"say\": \"Hello World\"}" op))))

(serve/servlet start
               #:launch-browser? #f
               #:quit? #f
               #:listen-ip #f
               #:port 8100
               #:extra-files-paths (list "e:\\hg\\MyWordsWeb")
               #:servlet-path "/send"
               #:command-line? #f
               #:stateless? #f)