#lang racket

;cond-expr/body: symbol
(define (loop-till cond-expr body)
  (unless (eval cond-expr)
    (eval body)
    (loop-till cond-expr body)))

;以下部分不能同loop-till一起初始化，而要在loop-till初始化后才repl中运行
;因为初始化后的define定义的变量会成为constant，不能再次被set!赋值
;总结:1.这种需求应该用宏来完成；2.使变量成为可变的（每次循环修改其值）而不是通过尾递归求值（每次循环产生新的变量值），需要权衡;
;(define i 0)
;(define res '())
;(loop-till '(> i 10)
;           '(let () (cons i res) (set! i (add1 i))))

;惰性求值，同样要在repl中执行，否则a,b的值不可更改
;(define a 0)
;(define b 0)
;(define sum (delay (λ () (+ a b))))