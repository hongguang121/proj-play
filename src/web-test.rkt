#lang racket

(require web-server/servlet
         web-server/servlet-env
         web-server/formlets
         web-server/formlets/syntax)

(provide/contract (start (request? . -> . response?)))

(define (start request)   
  (render-test-page "This is result zone" request))

(define content-formlet
  (formlet
   (#%# ,{input-string . => . content})
   (values content)))

(define (render-test-page res request)
  (define (response-generator embed/url)
    (response/xexpr
     `(html (head (meta ((charset "UTF-8")))
                  (meta ((content "width=device-width, initial-scale=1")
                         (name "viewport")))
                  (script ((src "js/jquery-3.1.1.js")))
                  (script ((src "js/bootstrap.js")))
                  (script ((src "js/react.js")))
                  (script ((src "js/react-dom.js")))
                  (script ((src "js/babel.min.js")))
                  (link ((href "css/bootstrap.css")
                         (rel "stylesheet")))
                  (link ((href "css/normalize.css")
                         (rel "stylesheet")))
                  (link ((href "css/buttons.css")
                         (rel "stylesheet")))
                  (link ((href "css/style.css")
                         (rel "stylesheet")))
                  (link ((rel "icon")
                         (type "image")
                         (href "img/favicon.png")
                         (size "32x32")))
                  (title "Test Page"))
            (body (h1 "Test Page")
                  (div ((id "test1"))
                       (form ((action
                               ,(embed/url click-button)))
                             ,@(formlet-display content-formlet))                             
                       (script ((src "js/ajax.js")))      
                       (button ((onclick
                                 "showres(\"Hi\")")
                                (class "btn btn-default find"))
                               (span ((class "glyphicon glyphicon-search"))
                                     )))
                  (br)
                  (form ()                                                                
                        (input ((id "ref")
                                (type "text")
                                (value "reflect"))))
                  (br)
                  (div ((id "result"))
                       ,res)
                  (script ((src "js/test.js")
                           (type "text/babel")))
                  ))))
  
  (define (click-button request)
    (let ((content (formlet-process content-formlet request)))
      (render-test-page content (redirect/get))))

  (define (q request)
    (render-test-page "Hi ajax" (redirect/get)))

  (if (eq? (request-uri request) "q")
      (send/suspend/dispatch q)
      (send/suspend/dispatch response-generator)))
  

(serve/servlet start
               #:launch-browser? #f
               #:quit? #f
               #:listen-ip #f
               #:port 8100
               #:extra-files-paths (list "e:\\source\\mywordsweb")
               #:servlet-path "/test"
               #:command-line? #f
               #:stateless? #f)