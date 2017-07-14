#lang racket

(require xml net/url)
(require web-server/servlet-env)

;; Both `server' and `accept-and-handle' change
;; to use a custodian.
;; parse html string 必须过滤掉/r/n
;; 通过(file->string)函数读取文件后，文件必须关闭掉，否则其他程序将无法写入

(define (serve port-no)
  (define main-cust (make-custodian))
  (parameterize ([current-custodian main-cust])
    (define listener (tcp-listen port-no 5 #t))
    (define (loop)
      (accept-and-handle listener)
      (loop))
    (thread loop)) ;建立线程，loop本身必须是一个thunk，否则它会在创建线程前就被求值
  (lambda ()
    (custodian-shutdown-all main-cust)))

(define (accept-and-handle listener)
  (define cust (make-custodian))
  (custodian-limit-memory cust (* 50 1024 1024))
  (parameterize ([current-custodian cust])
    (define-values (in out) (tcp-accept listener))
    (thread (lambda ()
              (handle in out)
              (close-input-port in)
              (close-output-port out))))
  ;; Watcher thread:
  (thread (lambda ()
            (sleep 10)
            (custodian-shutdown-all cust))))

#|
(define (handle in out)
  ;; Discard the request header (up to blank line):
  (regexp-match #rx"(\r\n|^)\r\n" in)
  ;; Send reply:
  (display "HTTP/1.0 200 Okay\r\n" out)
  (display "Server: k\r\nContent-Type: text/html\r\n\r\n" out)
  (display "<html><body>Hello, world!</body></html>" out))
|#

(define (handle in out)
  (define req
    ; Match the first line to extract the request:
    (regexp-match #rx"^GET (.+) HTTP/[0-9]+\\.[0-9]+"  ;GET请求
                  (read-line in)))
  (when req
    ; Discard the rest of the header (up to blank line):
    (regexp-match #rx"(\r\n|^)\r\n" in) ;在地址栏按下回车键
    ; Dispatch:
    (let ([xexpr (dispatch (list-ref req 1))])
      ; Send reply:
      (display "HTTP/1.0 200 Okay\r\n" out)
      (display "Server: k\r\nContent-Type: text/html\r\n\r\n" out)
      (display (xexpr->string xexpr) out))))

(define (dispatch str-path)
  ; Parse the request as a URL:
  (define url (string->url str-path))
  ; Extract the path part:
  (define path (map path/param-path (url-path url)))
  ; Find a handler based on the path's first element:
  (define h (hash-ref dispatch-table (car path) #f))
  (if h
      ; Call a handler:
      (h (url-query url))
      ; No handler found:
      `(html (head (title "Error"))
            (body
             (font ((color "red"))
                   "Unknown page: "
                   ,str-path)))))
 
(define dispatch-table (make-hash))

(hash-set! dispatch-table "hello"
           (lambda (query)
             `(html (body "Hello, World!"))))

(define ls '("Yaoer" "Li" "Chen"))

(hash-set! dispatch-table "ajaxreq"
           (lambda (query)
             `(html
               (head (meta ((charset "UTF-8")))
                     (script ((src "jquery-3.1.1.js"))))
               (body (button ((id "btn")) "Get file")
                     (h3 ((id "test")) "Click button")
                     (script
                            "$(document).ready(function(){
                                $('#btn').click(function(){
                                var res = $('#test').load('\text');
                                console.log(res);
                                })
                                 })")
                     (script "$(document).ready(function(){ $(\"#btn\").click(function(){ $(\"#test\").load(\"ajax.txt\") }) })")))))



(define (build-request-page label next-url hidden)
  `(html
    (head (title "Enter a Number to Add"))
    (body ([bgcolor "white"])
          (form ([action ,next-url] [method "get"])
                ,label
                (input ([type "text"] [name "number"]
                                      [value ""]))
                (input ([type "hidden"] [name "hidden"]
                                        [value ,hidden]))
                (input ([type "submit"] [name "enter"]
                                        [value "Enter"]))))))

(define (text query)
  (build-request-page "see text:" "/see-text" ""))

(define (see-text query) "Hi~")

(define (many query)
  (build-request-page "Number of greetings:" "/reply" ""))
 
(define (reply query)
  (define n (string->number (cdr (assq 'number query))))
  `(html (body ,@(for/list ([i (in-range n)])
                   " hello"))))
 
(hash-set! dispatch-table "many" many)
(hash-set! dispatch-table "reply" reply)
(hash-set! dispatch-table "text" text)
(hash-set! dispatch-table "see-text" see-text)

(define (sum query)
  (build-request-page "First number:" "/one" ""))
 
(define (one query)
  (build-request-page "Second number:"
                      "/two"
                      (cdr (assq 'number query))))
 
(define (two query)
  (let ([n (string->number (cdr (assq 'hidden query)))]
        [m (string->number (cdr (assq 'number query)))])
    `(html (body "The sum is " ,(number->string (+ m n))))))
 
(hash-set! dispatch-table "sum" sum)
(hash-set! dispatch-table "one" one)
(hash-set! dispatch-table "two" two)



;;启动服务器
(define stop (serve 8000))
;;关闭服务器 (stop)

;(xexpr->string '(html (head (title "Hello")) (body "Hi!")))
;(string->xexpr "<html><head><title>Hello</title></head><body>Hi!</body></html>")

(define u (string->url "http://localhost:8080/foo/bar?x=bye"))
;(url-path u)
;(url-query u)
