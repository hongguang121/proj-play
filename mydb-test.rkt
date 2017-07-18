#lang racket

(require db)

(define USER "root")
(define PASS "hikari57")
(define mydb   
  (mysql-connect #:user USER
                 #:password PASS
                 #:server "localhost"
                 #:port 3306
                 #:database "world"))

(define (search)
  (query-rows mydb
              "SELECT ID, Name, CountryCode, District, Population FROM city"))

(define (insert id name info)
  (query-exec mydb
              "INSERT INTO my_table (id, name, info) VALUES (?, ?, ?)"
              id name info))


;(filter (λ (x) (string=? (vector-ref x 2) "CHN")) (search))