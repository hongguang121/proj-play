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
  ;;(set! headers (request-headers request))
  ;;(set! headers (extract-binding/single 'access-control-request-method headers)) ;;下一次请求的方法
  ;;(displayln headers)
  ;;(displayln (bytes=? method #"OPTIONS"))

  ;;;;(set! req-info (request-post-data/raw request)) ;可以获得JSON.stringify()字符串化后ajax中data的数据
  ;;;(set! req-info (request-bindings request)) ;响应GET请求，内容是{key: value}形式
  ;(set! req-info (request-bindings/raw request))
  ;(set! req-info (cons (extract-binding/single 'name (request-bindings request)) req-info))
  ;(display req-info)

  (if (bytes=? method #"GET")
      (render-response-page "GET" request)  
  (if (bytes=? method #"OPTIONS")
      (render-response-page "OPTIONS" request)
      (let ()
        (if (eq? (caar (request-bindings request)) 'files)
            (begin
              (set! req-info (extract-binding/single 'files (request-bindings request))) ;通过key从FormData中取值
              (set! path (gen 'jpg (generator-path (ymd (current-date)))))) ;添加文件类型后缀名
            (begin
              (set! req-info (extract-binding/single 'videos (request-bindings request))) ;通过key从FormData中取值,需要append("name", "value"))
              (set! path (gen 'mp4 (generator-path (ymd (current-date))))))) ;添加文件类型后缀名
        (display-to-file req-info path #:mode 'binary #:exists 'replace) ;FormData 将图片/视频写入服务磁盘端存储
        (displayln (string-append (format-date (current-date)) " " path)) ;存储路径
        ;(displayln (entity-subtype (message-entity (mime-analyze req-info)))) ;MIME Type
        ;(displayln (entity-encoding (message-entity (mime-analyze req-info)))) ;MIME Type
        (render-response-page (string-append "http://172.16.9.64:8100" (substring path 7)) request))))))  

(define (render-response-page content request)
  (let ((resp (if (or (string=? content "OPTIONS")(string=? content "GET"))
                  (λ (op) (write-string "{\"say\": \"Hello World\"}" op))
                  (λ (op) (write-string content op)))))
    ;(displayln content)
    (response
     200 #"Moved Permanently"
     (current-seconds) ;TEXT/HTML-MIME-TYPE
     #"text/plain; charset=utf-8"
     ;#"application/octet-stream"
     (list (make-header #"Access-Control-Allow-Origin"
                        #"*"))
     resp)))
    
  
  ;(response/xexpr
     ;`(html
     ;  (head)
     ;  (body
     ;   (p "Hello")
        ;,(request-bindings/raw request)
     ;   ,(displayln content)))))
 ;(λ (op) (write-string "<html><body>Hello, World!</body></html>" op))))
 ;(λ (op) (write-string (string-append "{\"videoUrl\":" content "}") op)))) ;以text形式返回ajax data object
 ;(λ (op) (write-string (symbol->string (car content)) op))))
 ;(λ (op) (write-string (symbol->string content) op))))

 ;(λ (op) (write-string content op))))
 
 ;(λ (op) (map (λ (x) (write-string (string-append
 ;                                   (bytes->string/utf-8 (binding:form-value x))
 ;                                   " ")
 ;                                  op)) content))))
 
(serve/servlet start
               #:launch-browser? #f
               #:quit? #f
               #:listen-ip #f
               #:port 8400
               #:extra-files-paths (list "e:\\pics\\")
               #:servlet-path "/video"
               #:command-line? #f
               #:stateless? #f)