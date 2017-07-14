#lang racket

;及时地将刚学会的东西用语言文字表述出来对掌握它至关重要

#|continuation可以捕获当前过程运行的情况并返回其当前运行值
  是反向的lambda，将延续处的值带出该环境并保存，call/cc的lambda中的
  参数k是一个过程，类似于Y-combinator中的f,进行匿名递归时用来消除匿名参数，该k可以
  消除过程中的第一层匿名参数。
;;;;有误|#

;(call-with-values values list)可以将(values ...)求出的多重值进行计算

#|call/cc含有procedure p = (call/cc (λ (k) ...))和continuation k，p中可以是任意内容，
  但是无法传递到p外(如expression 1，会直接返回,而expression 2因为有k，故在k处中断)，
  只有传递给k的内容才可以传递出来；p内类似(let() ...)只有最后一个或存在副作用的表达式才会被返回
  (expression 3 和 expression 4)。那么k是什么？k就是传递给他的那个内容自身。|#

#|于是call/cc可以嵌入到任意过程的任意位置，从而组合出各式各样的功能应对各种情形|#

;;letcc可以从递归链中直接中断跳出

(define frozen '())
(define result '())

;expression 1
(call/cc (λ (k) (* 5 4))) ;=> (call/cc (λ (k) (k (* 5 4))))
;((λ (k) 6)(λ (k) 6)) ;6从一个匿名过程中返回

;expression 2
(call/cc (λ (k) (* 5 (k 4))))

;expression 3
(call/cc (λ (k) (set! frozen k) (* 5 4)))

;expression 4
(call/cc (λ (k) (display (* 5 4)) (set! frozen k)))

((call/cc (λ (k) (begin "p" (k (λ (x) x))))) 1)

(let ([x (call/cc (λ (k) k))]);k传递到p外，故x就是k
    (x (λ (ignore) "hi")))

;上式等价于一下3式
((call/cc (λ (k) k)) (λ (ignore) "hi"))
((call/cc (λ (k) (k (λ (ignore) "hi"))))(λ (ignore) "hi"))
((λ (ignore) "hi")(λ (ignore) "hi"));后ignore参数旨在传递给前ignore

(((call/cc (λ (k) k))
  (λ (fact)
    (λ (n)
      (if (= n 0)
          1
          (* n ((fact fact) (- n 1))))))) 6)

(((call/cc (λ (k) (k k)))
  (λ (fact)
    (λ (n)
      (if (= n 0)
          1
          (* n ((fact fact) (- n 1))))))) 6)

#|Scheme allows the continuation of any expression to be captured with the
procedure call/cc. call/cc must be passed a procedure p of one argument.
call/cc constructs a concrete representation of the current continuation
and passes it to p. The continuation itself is represented by a procedure
k. Each time k is applied to a value, it returns the value to the
continuation of the call/cc application. This value becomes, in essence,
the value of the application of call/cc.|#

#|The continuation captured by this invocation of call/cc may be described
as "Take the value, bind it to x, and apply the value of x to the value
of (lambda (ignore) "hi")." Since (lambda (k) k) returns its argument,
x is bound to the continuation itself; this continuation is applied to
the procedure resulting from the evaluation of (lambda (ignore) "hi").
This has the effect of binding x (again!) to this procedure and applying
the procedure to itself. The procedure ignores its argument and returns
"hi".|#

(call/cc (λ (k) ((λ (ignore) "hi") k))) ;k被ignore消费了，该call/cc等同于expression 1
(call/cc (λ (k) ((λ (x) "hi") k) (* 5 4)))
(call/cc (λ (k) (k 5) (* 5 4)))


((call/cc (λ (k) k)) (λ (ignore) (+ 1 2)))

((call/cc (λ (k) (k (λ (ignore) (+ 1 2))))) '1)

(((call/cc (λ (k) k)) (λ (x) x)) "HEY!")

(((λ (x) x)(λ (x) x)) "HEY!")

(define reciprocals
  (λ (ls f res)
    (cond ((null? ls) res)
          ((= (car ls) 0) (f "zero found"))
          (else (reciprocals (cdr ls) f (cons (/ 1 (car ls)) res))))))

(call-with-values (λ () (partition even? '(1 2 3 4 5 6)))
                    (λ (x y) (cons y (list x))))

(define-syntax head
  (syntax-rules ()
    [(_ expr)
     (call-with-values
      (λ () expr)
      (λ (x . y) x))]))

(head (values 1 2 3))

(let-values ([(a b) (partition even? '(1 2 3 4))])  a)

(let ((r (call/cc (λ (k) (k 5)))))
    (* 5 (+ 3 r)))

(let ((r (λ (continuation)
             (+ 1000 (continuation 6)))))
    (* (+ (call/cc r) 3) 8))

(let ((r (λ (continuation) (+ 1000 6))))
    (* (+ (call/cc r) 3) 8))

(let ((r (λ (continuation)
             (continuation
              (if (zero? (continuation (random 2)))
                  (+ 1000 6)
                  6)))))
    (+ (* (+ (call/cc r) 3) 8)
       (* (+ (call/cc r) 3) 8)))

(define-syntax generator
  (syntax-rules ()
    [(_ fun)
     (apply (car fun) (cdr fun))]))

(define (fibs n)
  (let loop ([a 0]
             [b 1]
             [c n]
             [res `(,values)])
    (if (> c 0)      
        (loop b (+ a b) (- c 1) (cons a res))
        (reverse res))))

(let-values ([(a b c d e) (generator (fibs 5))])
  (cons a e))

(let ((r (lambda (proc)
             (cons 'c (proc (cons 'd '()))))))
    (cons 'a (cons 'b (call/cc r))))

(define countdown
  (λ (n)
    (writeln "This only appears once")
    (let ((pair (message "Exit" (attempt (message "Enter" n)))))
      (let ((v (first pair))
            (returner (second pair)))
        (writeln (format "~a~a" "  The non-negative-number: " v))
        (if (positive? v)
            (returner (list (sub1 v) returner))
            (writeln "Blastoff"))))))

(define message
  (λ (direction value)
    (writeln (format "~a~a~a~a" "  " direction "ing attempt with value: " value))
    value))

(define attempt
  (λ (n)
    (let ((receiver (λ (proc) (list n proc))))
      ;(receiver (λ (x) x)))))
      (call/cc receiver))))

(apply (λ (k) k) (list (λ (x) "hi")))

(apply (call/cc (λ (k) k)) (list (λ (x) "hi")))

(apply (call/cc (λ (k) k)) (list (λ (x) x)))

(define rem "")

(define hello
    (λ ()
      (string-append "Hello "
                     (call/cc (λ (k) (set! rem k) "world")))))
(hello)
(rem "Yaoer")

;模拟js异步处理文件
(define f "file reader")
(define (read path)
    (thread (λ ()
              (set! f (cons (file->string path) f))
              (printf "read file success!"))))

;(let ()    
;  (read "e:/source/rkt_src/words-web.rkt")
;  (read "e:/source/rkt_src/scrib.rkt"))

(define foo '()) ;可以当做缓存，如果(fact 6)的值已知，想求(fact 7)，只用写(foo 7),如果想求(fact 20)，则通过循环就好

;factorial
(define (fact n . debug)
  (if (eq? #t (car debug))
      (let ((iter (λ () (printf "~a * (fact(~a) =\n" n (- n 1))))
            (end (λ () (printf "1)\n"))))
        (if (= n 0)      
            (call/cc (λ (k) (end) (set! foo k) (k 1)))
            (call/cc (λ (k)
                       (iter)
                       (k (* n (fact (- n 1) #t)))))))
      (if (= n 0)      
            (call/cc (λ (k) (set! foo k) (k 1)))
            (call/cc (λ (k) (k (* n (fact (- n 1) #f))))))))

;factorial cps
(define (fact-cps n ret)
  (call/cc (λ (k)
  (if (= n 0)
      (k ret) ;将ret带出作用域,但目前无法观测到ret的内部表达式，原因是内部表达式设计编译层面
      (fact-cps (- n 1)
                (λ (r)
                  (ret (* n r)))))))) ;第一次到此处时，ret是最初传入的ret，它将会跟随递归直到结束，并被最后执行，而每一次ret都会被上一层的ret更新，然后继续传递下去；
;((fact-cps 6 (λ (x) x)) 1)

;fact tail cps
;(fact-tail-cps 6 1 (λ (x) x))
(define (fact-tail-cps n r ret)  
  (if (= n 0)
      (ret r)
      (fact-tail-cps (- n 1) r (λ (r) (ret (* n r))))))

;fibonacci cps
(define (fib-cps n ret)
  (if (< n 2)
      (ret n)      
      (fib-cps (- n 1)
               (λ (v1)
                 (fib-cps (- n 2)
                          (λ (v2)
                            (ret (+ v1 v2))))))))

;fib tail cps
;(fib-tail-cps 0 1 10 (λ (x) x))
(define (fib-tail-cps a b n ret)
  (if (= n 0)
      (ret a)
      (fib-tail-cps b (+ a b) (- n 1) ret)))

(define r "continuation")
(call/cc (λ (k) (set! r k) (r "continuation is defined")))
(+ 5 (r ((λ () (add1 6)))))
((call/cc (λ (k) k)) (λ (x) (add1 6)))

(define new-escaper "any procedure")
(let ((receiver (λ (k)
                  (set! new-escaper
                        (λ (proc)
                          (λ args
                            (k (λ ()
                                 (apply proc args))))))
                  (λ () (writeln "new-escaper is defined")))))
  ((call/cc receiver)))

((new-escaper +) 1 2 3)

(define *-and-count-maker
  (λ ()
    (let ((local-counter 0))
      (λ (n1 n2)
        (set! local-counter (+ local-counter 1))
        (printf "Number of multiplications = ~a\n" local-counter)
        (* n1 n2)))))

(define product+
  (λ (n nums *-proc)
    (letrec
        ((product
          (λ (nums)
            (cond ((null? nums) 1)
                  ((number? (car nums))
                   (cond ((zero? (car nums)) 0)
                         (else (*-proc (car nums) (product (cdr nums))))))
                  (else (let ((val (product (car nums))))
                          (cond ((zero? val) 0)
                                (else (*-proc val (product (cdr nums)))))))))))
      (let ((prod (product nums)))
        (if (zero? prod) 0 (+ n prod))))))

(let ((counter (*-and-count-maker))
      (num-list '((1 2) (1 1 (3 1 1)) (((((1 1 0) 1) 4) 1) 1))))
  (product+ 5 num-list counter))

(let ((receiver (λ (continuation)
                  (call/cc continuation))))
  (call/cc receiver))

(define flatten-number-list
  (λ (s)
    (cond ((null? s) '())
          ((number? s) (list (break s)))
          (else (let ((flatcar (flatten-number-list (car s))))
                  (append flatcar
                          (flatten-number-list (cdr s))))))))

#|
(define break
  (λ (x)
    (let ((break-receiver
           (λ (k)
             (k x))))
      (call/cc break-receiver))))
|#

;(flatten-number-list '((1 2 3) ((4 5)) (6)))

(define get-back "any escape procedure")

(define break
  (λ (x)
    (let ((break-receiver
           (λ (k)
             (set! get-back (λ () (k x)))
             (any-action x))))
      (call/cc break-receiver))))

(define any-action
  (λ (x)
    ((new-escaper (λ () x)))
    (get-back)))



