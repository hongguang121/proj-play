#lang web-server/insta


(define (start request)
  (response/xexpr
   `(html
     (head (title "web study"))
     (body (h1 "Web Applications in Racket")
      (div (ol (li "先掌握基本原理")
               (li "投入实践")
               (li "融会贯通")
               (li "同前端技术结合")))))))
