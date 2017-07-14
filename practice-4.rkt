#lang racket

(require xml net/url)

(define in (open-input-file "e:/get.txt"))
(define in+ (open-input-file "e:/post.txt"))

;(read-line in)

(define req-get (regexp-match #rx"^GET (.+) HTTP/[0-9]+\\.[0-9]+" (read-line in)))
(define req-post (regexp-match #rx"^POST (.+) HTTP/[0-9]+\\.[0-9]+" (read-line in+)))

;when和unless:前者片段条件如果#t则顺序执行之后语句并返回最后一句的值，后者相反，如果条件为#f则顺序执行之后的语句并返回最后一句的值;

(regexp-match #rx"(\r\n|^)\r\n" in) ;跳过回车空格

(define str-path (list-ref req-get 1)) ;提取GET请求内容
(define str-path+ (list-ref req-post 1)) ;提取POST请求内容

(define url (string->url str-path)) ;将字符URL转换为URL

(define path (map path/param-path (url-path url))) ;以/为单位提取localhost:port/之后的内容

(define dispatch-table (make-hash)) ;建立hash-table

(hash-set! dispatch-table "hello"
           (lambda (query)
             (if (string=? (cdar query) "bye")
                 `(html (body "good bye!"))
                 `(html (body "Hello, World!"))))) ;创建响应页面，也可以在此处理逻辑

;((lambda (query)`(html (body "Hello, World!"))) '()) ；意味可以通过query来确定返回值

(define h (hash-ref dispatch-table (car path) #f)) ;h如果存在则等于(lambda (query) `(html (body "Hello, World!")))，是一个过程

(hash-ref dispatch-table "hello")

(url-query url) ;提取url中的query内容

(h (url-query url)) ; = (h '((x . "bye"))) 通过查询内容返回哈希表中"hello"的值

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
                                        [value "Enter"])))))) ;服务器端响应页面

;过滤掉html页面中的"/r/n"
(define register (file->string "e:/test.txt"))

(define (no-rn str)
  (string-replace str #rx"\r|\n|\t" ""))

;注意在<meta><link><input><img>等单标签末尾一定要有"/"，另外html开头不允许有\uFEFF;
;另外独立标签如<h1><p>等需要在<div>块中，<input><textarea>等表单标签一定要在表单中;
;多个独立<div>需要有外层<div>包裹，即必须要有根元素
;属性要加引号 string::13: read-xml: lex-error: at position 1.12/13: attribute values must be in ''s or in ""s
;需要实体引用:
;&lt; 	< 	小于
;&gt; 	> 	大于
;&amp; 	& 	和号
;&apos; 	' 	单引号
;&quot; 	" 	引号

(define (parse-xml str)
  (string->xexpr (substring (no-rn str) 1)))





