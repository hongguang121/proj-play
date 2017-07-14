#lang racket

(println "Hello World!")

(print "请输入您的名字：")

(define ln (read))

(format "您好！~A先生/女士。" ln)