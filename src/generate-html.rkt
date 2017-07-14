#lang web-server/insta

;按需求自动生成html

;(define (start request)
;  (response/xexpr
;   '(html
;     (head (title "My Blog"))
;     (body (h1 "Under construction")
;           (p "This is an example")
;           (a ((href "link.html")) "Past")
;           (p "This is " (div ((class "emph")) "another") " example.")
;           (ul (li "Larry")(li "Curly")(li "Moe"))))))

(struct post (title body))

(define BLOG (list (post "Second Post" "This is another post")
                   (post "First Post!" "Hey, this is my first post!")))

(define (start request)
  (define a-blog
    (cond ((can-parse-post? (request-bindings request))
           (cons (parse-post (request-bindings request))
                 BLOG))
          (else BLOG)))
  (render-blog-page a-blog request))

(define (can-parse-post? bindings)
  (and (exists-binding? 'title bindings)
       (exists-binding? 'body bindings)))

(define (parse-post bindings)
  (post (extract-binding/single 'title bindings)
        (extract-binding/single 'body bindings)))

(define (render-blog-page a-blog request)
  (response/xexpr
   `(html (head (title "My Blog"))
          (body (h1 "My Blog")
                ,(render-posts a-blog)
                (form
                 (input ((name "title")))
                 (input ((name "body")))
                 (input ((type "submit"))))))))

(define (render-post a-post)
  `(div ((class "post"))
        ,(post-title a-post)
        (p ,(post-body a-post))))

(define (render-posts a-blog)
  `(div ((class "posts"))
        ,@(map render-post a-blog)))

(define (render-greeting a-name)
  (response/xexpr
   `(html (head (title "Welcome"))
          (body (p ,(string-append "Hello " a-name))))))

(define (render-as-itemized-list fragments)
  `(ul ,@(map render-as-item fragments))) ;为了去掉因map产生的一对括号

(define (render-as-item a-fragment)
  `(li ,a-fragment))

