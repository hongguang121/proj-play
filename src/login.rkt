#lang racket

(require web-server/servlet
         web-server/servlet-env
         web-server/formlets
         web-server/formlets/syntax)

(require "words.rkt")

(provide/contract (start (request? . -> . response?)))

(define (start request)
  (render-user-page "check" request))

#|(define check-user-formlet
  (formlet
   (div "User name: " ,{input-string . => . name}
        "User Pass: " ,((password-input) . => . password))
   (list name (to-string (required password)))))|#

(define (render-user-page res request)
  (define (response-generator embed/url)
    (response/xexpr
     `(html (head (title "User Login"))
            (body (h1 "Login")
                  (form ((action
                          ,(embed/url parse-user))
                         (method "post"))
                        ;,@(formlet-display check-user-formlet)
                        "User:"(br)
                        (input ((name "name")))
                        "Password:"(br)
                        (input ((name "password") (type "password")))
                        " "
                        (input ((type "submit"))))
                  (div ,res)))))

  (define (parse-user request)
    (let (;(user (formlet-process check-user-formlet request))
          (name (extract-binding/single 'name (request-bindings request)))
          (password (extract-binding/single 'password (request-bindings request)))
          (res ""))
      (if (eq? '() (search-u (list name password)))
          (begin
            (set! res "用户名或密码错误")
            (render-user-page res request))
          (begin
            (set! res "登录成功")
            (redirect-to "/visual.html")))))
            ;(render-user-page res request)))))
  
  (send/suspend/dispatch response-generator))


(serve/servlet start
               #:launch-browser? #f
               #:quit? #f
               #:listen-ip #f
               #:port 8000
               #:extra-files-paths (list "e:\\source\\MyWordsWeb")
               #:servlet-path "/login"
               #:command-line? #f
               #:stateless? #f)