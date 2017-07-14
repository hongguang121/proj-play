#lang racket

(require web-server/servlet
         web-server/servlet-env
         web-server/formlets
         web-server/formlets/syntax
         web-server/formlets/input
         net/mime)

(require 2htdp/image
         racket/cmdline
         (only-in racket/draw read-bitmap))

(provide/contract (start (request? . -> . response?)))

(define (start request)
  (render-intro-page "" "img/java.png" request))

(define new-intro
  (formlet
   (div " title: " ,{input-string . => . title}
        " article: " ,{(to-string (required (textarea-input))) . => . article}
        " upload: " ,{(to-string (required (file-upload))) . => . file}
        (input ((type "submit") (value "提交"))))
   (list title article file)))

(define (render-intro-page res pic request)
  (define (response-generator embed/url)
    (response/xexpr
     `(html (head (meta ((charset "UTF-8")))
                  (script ((src "js/jquery-3.1.1.js")))
                  (script ((src "js/bootstrap.js")))
                  (link ((href "css/style.css")
                         (rel "stylesheet"))))
            (body (h1 "编辑文本")
                  (form ((action
                          ,(embed/url parse-intro))
                          (method "post"))
                          ;(enctype "multipart/form-data"))
                        ,@(formlet-display new-intro))
                  (div ((id "result"))
                       (img ((src ,pic) (width "150")))
                       ,res)))))
    (define (parse-intro request)
      (let ((res (formlet-process new-intro request)))
        ;(define f (file->string (string-append "e:\\source\\MyWordsWeb\\" (caddr res))))
        ;(define f (file->string (caddr res)))
        ;(write-to-file f "e:/new.txt" #:mode 'text)
        (render-intro-page (string-append (car res) ": " (cadr res)) "img/clojure-logo-120b.png" (redirect/get))))
    (send/suspend/dispatch response-generator))

(serve/servlet start
               #:launch-browser? #f
               #:quit? #f
               #:listen-ip #f
               #:port 8100               
               #:extra-files-paths (list "e:\\source\\MyWordsWeb")
               #:servlet-path "/intro"
               #:command-line? #f
               #:stateless? #f)