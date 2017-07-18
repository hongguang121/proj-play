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
  (set! headers (request-headers request))
  ;;(set! headers (extract-binding/single 'access-control-request-method headers)) ;;下一次请求的方法
  ;;(displayln method)
  ;;(displayln headers)
  ;;(displayln (bytes=? method #"OPTIONS"))

  ;;;;(set! req-info (request-post-data/raw request)) ;可以获得JSON.stringify()字符串化后ajax中data的数据
  ;;;(set! req-info (request-bindings request)) ;响应GET请求，内容是{key: value}形式
  ;(set! req-info (request-bindings/raw request))
  ;(set! req-info (cons (extract-binding/single 'name (request-bindings request)) req-info))

  ;(displayln (request-bindings request)) ;查看请求结构

  (if (bytes=? method #"GET")
      (render-response-page (send-json (file->string "e:/list.json")) "GET" request)
  (if (bytes=? method #"OPTIONS")
      (render-response-page (string->jsexpr "{\"say\": \"Hello World\"}") "OPTIONS" request)
  (if (and (bytes=? method #"POST")
           (null? (request-bindings request)))
      (render-response-page (string->jsexpr "{\"say\": \"Hello World\"}") "POST" request)
      (let ()       
        (if (eq? (caar (request-bindings request)) 'files) ;上传的图片格式是FormData对象.append("files", blob)
            (begin
              ;多张图片上传
              (set! req-info
                    (map
                     (λ (img) (cons (cdr img) (gen 'jpg (generator-path (ymd (current-date)))))) ;(cons 图片编码 . 存储路径)
                     (request-bindings request))) ;request是图片编码集合
              
              (map (λ (info)
                     (begin
                       (display-to-file (car info) (cdr info) #:mode 'binary #:exists 'replace) ;写入磁盘
                       (displayln (string-append (format-date (current-date)) " " (cdr info))) ;显示路径日志
                       (set! path (cons (cdr info) path)) ;路径集合
                       ))
                   req-info)

              (render-response-page "http://172.16.9.64:8100" (reverse path) request)

              ;(displayln path)
              
              ;(set! req-info (extract-binding/single 'files (request-bindings request))) ;通过key从FormData中取值 ;req-info此时是图片编码列表 ;单张上传             
              ;(set! path (gen 'jpg (generator-path (ymd (current-date)))))) ;添加文件类型后缀名
              ) 
            (begin
              (set! req-info (extract-binding/single 'videos (request-bindings request))) ;通过key从FormData中取值,需要append("name", "value"))
              (set! path (list (gen 'mp4 (generator-path (ymd (current-date)))))) ;添加文件类型后缀名
              (display-to-file req-info (car path) #:mode 'binary #:exists 'replace) ;FormData 将图片/视频写入服务磁盘端存储
              (displayln (string-append (format-date (current-date)) " " (car path))) ;存储路径
              (render-response-page "http://172.16.9.64:8100" (reverse path) request)
              ))))))
        
        ;(displayln (entity-subtype (message-entity (mime-analyze req-info)))) ;MIME Type
        ;(displayln (entity-encoding (message-entity (mime-analyze req-info)))) ;MIME Type
       
        ))

(define (render-response-page init content request)
  (let ((resp (if (or (eq? content "OPTIONS")(eq? content "GET")(eq? content "POST"))
                  (λ (op) (write-json init op))
                  (λ (op) (write-json (send-path init content) op)))))
    ;(displayln content)
    (response
     200 #"Moved Permanently"
     (current-seconds) ;TEXT/HTML-MIME-TYPE
     #"text/plain; charset=utf-8"
     ;#"application/octet-stream"
     (list (make-header #"Access-Control-Allow-Origin"
                        #"*"))
     resp)))

;将路径集合拼接成json字符串
(define (send-path init path)
  ;(displayln path)
  (unless (null? path)
    (let loop ((p path)
               (str "{\"path\": ["))    
      (if (string=? (last path) (car p))
          (string->jsexpr (string-append str "\"" init (substring (car p) 7) "\"]}"))
          (loop (cdr p)
                (string-append str "\"" init (substring (car p) 7) "\","))))))

;返回GET请求的json数据
(define (send-json data)
  (string->jsexpr data))
  
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
               #:port 8100
               #:extra-files-paths (list "e:\\pics\\")
               #:servlet-path "/upload"
               #:command-line? #f
               #:stateless? #f)