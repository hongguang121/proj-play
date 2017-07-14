#lang racket

(require web-server/servlet
         web-server/servlet-env
         web-server/formlets
         web-server/formlets/syntax)

(require "words.rkt")

(provide/contract (start (request? . -> . response?)))

(define (start request)
  (render-words-page "" request))

(define new-word-formlet
  (formlet
   (#%# ,{input-string . => . spell})
   (values spell)))


(define iter-formlet
  (formlet* `(div ,@(for/list ([i (in-range 1 10)])
                    `(p ,(number->string i)
                        ,((text-input) . =>* . name))))
          name))

(define date-formlet
  (formlet
   (div "Month:" ,{input-int . => . month}
        "Day:" ,{input-int . => . day}
        " " (input ((type "submit"))))
   (list month day)))

(define (render-words-page res request)
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
                  ;(script ((src "js/ajax.js")))
                  (link ((href "css/bootstrap.css")
                         (rel "stylesheet")))
                  (link ((href "css/normalize.css")
                         (rel "stylesheet")))
                  (link ((href "css/style.css")
                         (rel "stylesheet")))
                  (title "Quickly Reading"))
            (body (h1 "My words list")
                  ;,(render-words a-list)
                  (div ((id "trans"))
                  (form ((action
                          ,(embed/url parse-word))
                         (method "post"))
                        "查询： "
                        ,@(formlet-display new-word-formlet)                                                
                        ;(input ((name "spell")))
                        " "
                        (input ((type "submit")))))                  
                  (form ((action
                          ,(embed/url parse-date)))
                        ,@(formlet-display date-formlet))
                  (br)
                  "录入文章： "
                  (form ((action
                          ,(embed/url translate)))                        
                        (textarea ((name "article")
                                   (style "width:300px;height:300px;margin-left:100px;")))
                        " "(input ((type "submit"))))
                  (br)
                  ,res
                  ;(form ()
                  ;      ,@(formlet-display iter-formlet))
                  
                  ;(div ((id "example")) "显示结果")
                  ;(script ((src "js/test.js")
                  ;         (type "text/babel")))
                  (script "function trans(){ document.getElementById(\"trans\")")
                  ;(script "$(document).ready(function(){ $(\"#ajax\").click(function(){ $(\"#ajaxreply\").html(\"<p>Hi</p>\") }) })")
                  (script "$(document).ready(function(){ $(\"#ajax\").click(function(){ $(\"#ajaxreply\").load(\"ajax.txt\") }) })")
                  (script "function hit(str) { document.getElementById(\"ajaxreply\").innerHTML = str; }")
                  (input ((type "text")
                          (onkeyup "hit(this.value)")))
                  (button ((id "ajax")) "ajax")
                  (div ((id "ajaxreply"))
                       "This part return a ajax request")
                  ))))

  (define (clickme request)
    (render-words-page "Hi, I'm back!" (redirect/get)))
    
  (define (parse-date request)
    (let ((month (number->string (car (formlet-process date-formlet request))))
          (day (number->string (cadr (formlet-process date-formlet request)))))
        (render-words-page `(p "Month:" ,month " Day:" ,day) (redirect/get))))


  (define (parse-word request)
    (let ((word (formlet-process new-word-formlet request)))
           ;(extract-binding/single 'spell (request-bindings request))))
      (if (string=? word "")
          (render-words-page '(p "请输入单词") (redirect/get))
          (let ((res (render-res (search-w word)))) ;注意，通过数据库查找可以实时更新
            (render-words-page (append '(p "查询结果：") (list res)) (redirect/get))))))
  (define (translate request)
    (let ((art (extract-binding/single 'article (request-bindings request))))
      (if (string=? art "")
          (render-words-page '(p "请录入文章") (redirect/get))
          (let ((res (dsp (analyse art))))
            (render-words-page res (redirect/get))))))  
  (send/suspend/dispatch response-generator))

(define (render-word a-word)
  `(div ((class "word"))
        (table ((border="1"))
         (tr
         (td ,(word-spell a-word))
         (td (p ,(word-mean a-word)))))))

(define (render-words a-list)
  `(div ((class "words"))
        ,@(map render-word (vocabulary-words a-list))))

(define (render-find a-list)
  `(div ((class "find"))
        (p ,(vector-ref a-list 0));,(car a-list))
        (p "释义：" ,(vector-ref a-list 1))));,(cadr a-list))))

(define (render-res lst)
  `(div ((class "result"))
        ,@(map render-find lst)))

(serve/servlet start
               #:launch-browser? #f
               #:quit? #f
               #:listen-ip #f
               #:port 8100
               ;#:max-waiting 900000
               #:extra-files-paths (list "e:\\source\\MyWordsWeb")
               #:servlet-path "/"
               #:command-line? #f
               #:stateless? #f)

