#lang racket

(require web-server/servlet
         web-server/servlet-env
         web-server/formlets
         web-server/formlets/syntax)

(require racket/date)
(require net/url)
(require json)
(require "setInterval.rkt")
(require "check-file.rkt")

(provide/contract (start (request? . -> . response?)))

;(setInterval (λ () (load-file "e:/good.txt")) 5)
;(setInterval (λ () (displayln "e:/good.txt")) 5)

(define *ls* '("Hello" "World" "This" "is" "a" "reactive" "app"))

(define (start request)
  (if (null? *ls*)
      (render-response-page "" request)
      (let ((message (car *ls*)))
        (set! *ls* (cdr *ls*))
        (render-response-page message request))))

(define (render-response-page res request)
#|  (response/xexpr
   '(html
     (head (title "reactive"))
     (body (h1 "react")))))|#
  (if (string=? res "")
    (response
     200 #"Moved Permanently"
     (current-seconds) ;TEXT/HTML-MIME-TYPE
     #"text/plain; charset=utf-8"
     ;#"application/octet-stream"
     (list (make-header #"Access-Control-Allow-Origin"
                        #"*"))
     (λ (op) (write-string "{\"result\": \"success\"}" op)))
    (response
     200 #"Moved Permanently"
     (current-seconds) ;TEXT/HTML-MIME-TYPE
     #"text/plain; charset=utf-8"
     ;#"application/octet-stream"
     (list (make-header #"Access-Control-Allow-Origin"
                        #"*"))
     (λ (op) (write-json (string->jsexpr (string-append "{\"result\":" "\"" res "\"" "}")) op)))))

(serve/servlet start
               #:launch-browser? #f
               #:quit? #f
               #:listen-ip #f
               #:port 8100
               #:extra-files-paths (list "e:\\")
               #:servlet-path "/reactive"
               #:command-line? #f
               #:stateless? #f)