#lang racket

;; See "<<<" for two small changes, then jump down
;; to `send/suspend'.

(require xml net/url
         racket/control) ;; <<< new import

(define (serve port-no)
  (define main-cust (make-custodian))
  (parameterize ([current-custodian main-cust])
    (define listener (tcp-listen port-no 5 #t))
    (define (loop)
      (accept-and-handle listener)
      (loop))
    (thread loop))
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

(define (handle in out)
  (define req
    ;; Match the first line to extract the request:
    (regexp-match #rx"^GET (.+) HTTP/[0-9]+\\.[0-9]+"
                  (read-line in)))
  (when req
    ;; Discard the rest of the header (up to blank line):
    (regexp-match #rx"(\r\n|^)\r\n" in)
    ;; Dispatch:
    (let ([xexpr (prompt (dispatch (list-ref req 1)))]) ;; <<< changed
      ;; Send reply:
      (display "HTTP/1.0 200 Okay\r\n" out)
      (display "Server: k\r\nContent-Type: text/html\r\n\r\n" out)
      (display (xexpr->string xexpr) out))))

(define (dispatch str-path)
  ;; Parse the request as a URL:
  (define url (string->url str-path))
  ;; Extract the path part:
  (define path (map path/param-path (url-path url)))
  ;; Find a handler based on the path's first element:
  (define h (hash-ref dispatch-table (car path) #f))
  (if h
      ;; Call a handler:
      (h (url-query url))
      ;; No handler found:
      `(html (head (title "Error"))
             (body
              (font ((color "red"))
                    "Unknown page: "
                    ,str-path)))))

(define dispatch-table (make-hash))

(hash-set! dispatch-table "hello"
           (lambda (query)
             `(html (body "Hello, World!"))))

(hash-set! dispatch-table "poem"
           (lambda (query)
             `(html
               ((lang "cn"))
               (head
                ()        
                (meta ((charset "utf-8")))
        
                (meta ((content "IE=edge") (http-equiv "X-UA-Compatible")))
        
                (meta
                 ((content
                   "width=device-width, initial-scale=1, maximum-scale=1, user-scalable=no")
                  (name "viewport")))
        
                (script ((src "js/jquery-3.1.1.js")))
        
                (script ((src "js/bootstrap.js")))
        
                (script ((src "js/buttons.js")))
        
                ;(script ((src "js/jquery.imagecompress.js")))
        
                (script ((src "js/poem.js")))
        
                (link ((href "css/bootstrap.css") (rel "stylesheet")))
        
                (link ((href "css/poem.css") (rel "stylesheet")))
        
                (link ((href "css/buttons.css") (rel "stylesheet")))
        
                (title () "诗与远方"))
               (body
                ((bgcolor ""))
                (div ((class "banner")))
                (nav
                 ((class "navbar navbar-default navset") (role "navigation"))
         
                 (div
                  ((class "container-fluid"))
          
                  (div
                   ((class "row"))
           
                   (div
                    ((class "navbar-header col-md-2"))
            
                    (button
                     ((class "navbar-toggle collapsed")
                      (data-target "#bs-example-navbar-collapse-1")
                      (data-toggle "collapse")
                      (type "button"))
             
                     (span ((class "sr-only")) "Toggle navigation")
             
                     (span ((class "icon-bar")))
             
                     (span ((class "icon-bar")))
             
                     (span ((class "icon-bar")))
                     )
            
                    (a ((class "navbar-brand") (href "#")) "诗与远方"))
           
                   (div
                    ((class "collapse navbar-collapse col-md-offset-7 col-md-1 navbar-set")
                     (id "nav-font"))
            
                    (ul
                     ((class "nav navbar-nav navbar-right"))
             
                     (li ())
                     )
                    )
           
                   (div ((class "head-icon")))
           
                   (div
                    ((class "collapse navbar-collapse col-md-offset-5 col-md-3 message"))
            
                    (div ((class "part-1")) (a ((href "#")) "标识展示"))
            
                    (div
                     ((class "part-2"))
             
                     (span () (a ((href "#")) "消息通知"))
             
                     (a
                      ((href "#"))
                      (span
                       ((aria-hidden "true")
                        (class "glyphicon glyphicon glyphicon-log-out part-3")
                        (style "padding-left:30px;"))))
                     )
                    )
                   )
                  ))
                (div
                 ((class "user"))
         
                 (div
                  ((class "head"))
          
                  (ol
                   ((class "breadcrumb"))
           
                   (li () (a ((href "#") (id "home")) "Home"))
           
                   (li () (a ((href "#") (id "lib")) "Library"))
           
                   (li ((class "active") (id "data")) "Data")
                   )
                  )
         
                 (div
                  ((class "left-panel"))
          
                  (div
                   ((class "input-group search"))
           
                   (input ((class "form-control") (placeholder "Search for...") (type "text")))
           
                   (span
                    ((class "input-group-btn"))
            
                    (button ((class "btn btn-default") (type "button")) "Search")
                    )
                   )
          
                  (br ())
                  (br ())
                  (br ())
          
                  (span ((id "edit-text")) " 文章目录 ")
          
                  (div
                   ((class "list-group article-list"))
           
                   (div ((class "art-pic")) (img ((src "img/pic/background.jpg") (width "200"))))
           
                   (div ((class "art-title")) "文章标题1")
                   )
          
                  (div
                   ((class "list-group article-list"))
           
                   (div ((class "art-pic")) (img ((src "img/pic/p2233951898.jpg") (width "200"))))
           
                   (div ((class "art-title")) "文章标题2")
                   )
          
                  (div
                   ((class "list-group article-list"))
           
                   (div ((class "art-pic")) (img ((src "img/pic/p2392823566.jpg") (width "200"))))
           
                   (div ((class "art-title")) "文章标题2")
                   )
          
                  (div
                   ((class "bt-art-lr"))
           
                   (button
                    ((class "button button-border button-pill button-small"))
                    (span ((aria-hidden "true") (class "glyphicon glyphicon-menu-left"))))
           
                   (button
                    ((class "button button-border button-pill button-small"))
                    (span ((aria-hidden "true") (class "glyphicon glyphicon-menu-right"))))
                   )
                  )
         
                 (div
                  ((class "middle-panel"))
          
                  (div
                   ((class "title"))
           
                   (div
                    ()
                    (input
                     ((class "init-title")
                      (cols "100")
                      (id "title")
                      (maxlength "100")
                      (name "title")
                      (placeholder "请输入标题")
                      (size "30")
                      (type "text")
                      (wrap "hard")
                      (onmouseover "this.focus();this.select()")
                      (style "border:none;color:'#363636';"))))
           
                   (div
                    ()
                    (input
                     ((class "init-auth")
                      (cols "25")
                      (id "auth")
                      (maxlength "25")
                      (name "auth")
                      (placeholder "请输入作者")
                      (size "25")
                      (type "text")
                      (wrap "hard")
                      (onmouseover "this.focus();this.select()")
                      (onclick "if(value==defaultValue){value='';this.style.color='#363636'}")
                      (onBlur "if(!value){value=defaultValue;this.style.color='#999'}")
                      (style "border:none"))))
           
                   (div ((id "count-auth")) (p () "0/25")))
          
                  (div
                   ((class "contents"))
           
                   (textarea
                    ((class "init-text")
                     (cols "30")
                     (id "summary")
                     (maxlength "600")
                     (name "summary")
                     (placeholder "请输入文章摘要/限500字内/可以不填")
                     (size "30")
                     (type "text")
                     (wrap "hard"))
                    )
           
                   (textarea
                    ((class "init-text")
                     (cols "30")
                     (id "text_0")
                     (maxlength "5000")
                     (name "content")
                     (placeholder "请输入正文")
                     (size "30")
                     (type "text")
                     (wrap "hard"))
                    )
           
                   (div ((id "count-cont")) (p ((id "cnt")) "0/5000"))
           
                   (div ((class "show_upload")))

                   (div ((class "image-upload"))
                        (input ((name "upload") (type "file") (multiple "") (class "image"))))
           
                   (div ((class "five")) (p () "一次限上传5张"))

                   #|
           (script "$('.image').imageCompress({
                        'quality': 50,
                        'onloadStart': function(result){
                            console.log('读取图片开始'+result);
                        },
                        'onloadEnd': function(result){
                            console.log('读取图片结束'+result);
                        },
                        'oncompressStart': function(result){
                            console.log('压缩图片开始'+result);
                        },
                        'oncompressEnd': function(result){
                            console.log('压缩图片结束'+result);
                            $('#preview').append(result);
                            $('#preview').find('img').addClass('preview');
                        },
                        'callback': function(){
                            console.log('处理完毕');
                        }
                    });")
           |#
           
                   (div ((class "link")))
           
                   (div
                    ((class "add-bt"))
            
                    (button
                     ((class "button button-pill button-border button-small")
                      (id "add_art")
                      (type "button"))
                     "添加文章段落")
            
                    (button
                     ((class "button button-pill button-border button-small")
                      (id "add_img")
                      (type "button"))
                     "添加新的图片")
                    )
           
                   (div
                    ((class "submit-bt"))
            
                    (input
                     ((class "button button-pill button-primary button-border button-small")
                      (id "final")
                      (type "submit")
                      (value "提交")))
                    )
                   )
                  )
         
                 (div
                  ((class "right-panel"))
          
                  (span ((id "add-media")) " 文本编辑 ")
          
                  (div
                   ((class "list-group right-list"))
           
                   (a
                    ((class "list-group-item lg t") (href "#"))
                    (span ((aria-hidden "true") (class "glyphicon glyphicon-pencil pic")))
                    (span ((class "text")) "预览"))
           
                   (a
                    ((class "list-group-item lg p") (href "#"))
                    (span ((aria-hidden "true") (class "glyphicon glyphicon-picture pic")))
                    (span ((class "text")) "编辑"))
                   )
          
                  (span ((id "add-text")) " 商品列表 ")
                  (br ())
          
                  (div ((class "panel-heading goods-list")) )
          
                  (div
                   ((class "bt-goods-lr"))
           
                   (button
                    ((class "button button-border button-pill button-small bgl"))
                    (span ((aria-hidden "true") (class "glyphicon glyphicon-menu-left"))))
           
                   (button
                    ((class "button button-border button-pill button-small bgr"))
                    (span ((aria-hidden "true") (class "glyphicon glyphicon-menu-right"))))
                   )
                  ))
                (div ((class "footer"))
                     (div ((id "res"))))))))

;; ----------------------------------------

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

(define (many query)
  ;; Create a page containing the form:
  (build-request-page "Number of greetings:" "/reply" ""))

(define (reply query)
  ;; Extract and use the form results:
  (define n (string->number (cdr (assq 'number query))))
  `(html (body ,@(for/list ([i (in-range n)])
                   " hello"))))

(hash-set! dispatch-table "many" many)
(hash-set! dispatch-table "reply" reply)

;; ----------------------------------------
;; Old, awkward version:

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

;; ----------------------------------------

;; Helper to grab a computation and generate a handler for it:

(define (send/suspend mk-page)
  (let/cc k
    (define tag (format "k~a" (current-inexact-milliseconds)))
    (hash-set! dispatch-table tag k)
    (abort (mk-page (string-append "/" tag)))))

;; Helper to run the number-getting page via `send/suspend':

(define (get-number label)
  (define query
    ;; Generate a URL for the current computation:
    (send/suspend
     ;; Receive the computation-as-URL here:
     (lambda (k-url)
       ;; Generate the query-page result for this connection.
       ;; Send the query result to the saved-computation URL:
       (build-request-page label k-url ""))))
  ;; We arrive here later, in a new connection
  (string->number (cdr (assq 'number query))))

;; ----------------------------------------

;; New direct-style servlet:

(define (sum2 query)
  (define m (get-number "First number:"))
  (define n (get-number "Second number:"))
  `(html (body "The sum is " ,(number->string (+ m n)))))

(hash-set! dispatch-table "sum2" sum2)

(define stop (serve 8000))