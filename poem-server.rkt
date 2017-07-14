#lang racket

(require web-server/servlet
         web-server/servlet-env
         web-server/formlets
         web-server/formlets/syntax)

(require racket/date)
(require file/sha1)
(require net/base64)
(require net/mime)
(require net/url)
(require json)
(require "gen-path.rkt")
(provide/contract (start (request? . -> . response?)))

(define (start request)
  (let ((headers null)
        (method null)
        (req-info null)
        (path null))
    
    (set! method (request-method request))
    
    (if (bytes=? method #"POST")
        (begin
          ;(display (request-bindings request))
          (set! req-info (request-bindings request))
          (set! path (gen 'txt (generator-path (ymd (current-date)))))
          (display-to-file (cdr (assq 'contents req-info)) path #:mode 'binary #:exists 'replace)
          (render-response-page req-info method request))
        (begin
          (set! req-info (file->string "e:/content.txt"))
          (render-response-page req-info method request)))))

(define (render-response-page content method request)
  (let ((resp (if (bytes=? method #"POST")                  
                  (λ (op) (write-string "success" op))
                  (λ (op) (write-string content op)))))
    ;(display content)
    (response
     200 #"Moved Permanently"
     (current-seconds) ;TEXT/HTML-MIME-TYPE
     #"text/plain; charset=utf-8"
     ;#"application/octet-stream"
     (list (make-header #"Access-Control-Allow-Origin"
                        #"*"))
     resp)))

(serve/servlet start
               #:launch-browser? #f
               #:quit? #f
               #:listen-ip #f
               #:port 8400
               #:extra-files-paths (list "e:\\pics\\")
               #:servlet-path "/poem"
               #:command-line? #f
               #:stateless? #f)