#lang racket

(require web-server/servlet
         web-server/servlet-env
         web-server/formlets
         web-server/formlets/syntax)

(require racket/date)
(require net/url)
(require json)

(provide/contract (start (request? . -> . response?)))

(define (start request)
  ())

(serve/servlet start
               #:launch-browser? #f
               #:quit? #f
               #:listen-ip #f
               #:port 8800
               #:extra-files-paths (list "e:\\")
               #:servlet-path "/chat"
               #:command-line? #f
               #:stateless? #f)