#lang racket

(require web-server/servlet
         web-server/servlet-env
         web-server/formlets
         web-server/formlets/syntax)

(require json)

(provide/contract (start (request? . -> . response?)))

(define req-info '())
(define content '())

(define (start request)
  (set! req-info (request-post-data/raw request))
  ;(set! content
  ;      (list `(title ,(hash-ref (string->jsexpr (bytes->string/utf-8 req-info)) 'title))
  ;            `(preferredPic ,(hash-ref (string->jsexpr (bytes->string/utf-8 req-info)) 'preferredPic))
  ;            `(labelType ,(hash-ref (string->jsexpr (bytes->string/utf-8 req-info)) 'labelType))
  ;            `(contents ,(hash-ref (string->jsexpr (bytes->string/utf-8 req-info)) 'contents))))
  (set! req-info (extract-binding/single 'title (request-bindings request)))
  ;(set! req-info (request-bindings request))
  ;(set! req-info (request-bindings/raw request))
  ;(set! req-info (extract-bindings 'title request))
  (render-response-page req-info request))

(define (render-response-page content request)
;    (response/xexpr
;     `(html
;       (head)
;       (body
;        (p "Hello")       
        ;,(display-to-file content "e:/test.txt" #:exists 'replace)
        ;,(displayln (string->jsexpr (bytes->string/utf-8 content)))        
;        ))))
  
  (response
   200
   #"Moved Permanently"
   ;content
 (current-seconds)
 TEXT/HTML-MIME-TYPE
 ;application/json-MIME-TYPE
 (list (make-header #"Access-Control-Allow-Origin"
                    #"*"))
 ;(λ (op) (map (λ (x) (write-string (string-append
 ;                                   (bytes->string/utf-8 (binding:form-value x))
 ;                                   " ")
 ;                                  op)) content))

 ;(λ (op)
 ;  (map (λ (e) 
 ;         (write-string (cadr e) op))
 ;       content))))

 ;(λ (op) (write-string (cadar content)) op)))
 (λ (op) (write-string req-info op))))

 ;(λ (op) (write-string (jsexpr->string (bytes->string/utf-8 req-info))) op)))

 ;(λ (op) (write-string "success") op)))
 
 

; (map (λ (x) (list (bytes->string/utf-8 (binding:form-value x)))) content)))
   ;(write-string content op))))

(serve/servlet start
               #:launch-browser? #f
               #:quit? #f
               #:listen-ip #f
               #:port 8100
               #:extra-files-paths (list "e:\\hg\\MyWordsWeb")
               #:servlet-path "/sendback"
               #:command-line? #f
               #:stateless? #f)