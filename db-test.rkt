#lang racket
(require db)

(define USER "root")
(define PASS "hikari57")
(define mydb   
  (mysql-connect #:user USER
                 #:password PASS
                 #:database "test"))

(define (insert-user name password)
  (thread (λ ()
            (query-exec mydb
                        "INSERT INTO user_test (name, password) VALUES (?, ?)"
                        name password))))

;(map (λ (x) (insert-user (first x) (second x))) users)

(define (search-user name)
  (query-rows mydb
              "SELECT name FROM user_test WHERE name = ?"
              name))

(define (show-users)
  (query-rows mydb "SELECT * FROM user_test"))

(define users '(("Yaoer" "hikari57")
                ("Hg" "hikaruom")
                ("Haoxue" "haopengyou")))
