#lang typed/racket

(: square (-> Real Real))
(define (square x) (* x x))

(: fact (-> Integer Integer))
(define (fact n)
    (if (= n 0)
        1
        (* n (fact (- n 1)))))

(define-type Tree (U leaf node))
(struct leaf ([val : Number]))
(struct node ([left : Tree] [right : Tree]))
 
(: tree-height (-> Tree Integer))
(define (tree-height t)
  (cond [(leaf? t) 1]
        [else (max (+ 1 (tree-height (node-left t)))
                   (+ 1 (tree-height (node-right t))))]))
 
(: tree-sum (-> Tree Number))
(define (tree-sum t)
  (cond [(leaf? t) (leaf-val t)]
        [else (+ (tree-sum (node-left t))
                 (tree-sum (node-right t)))]))

(: assert-symbols! ((Listof Any) -> (Listof Symbol)))
(define (assert-symbols! lst)
  (match lst
    [(list (? symbol? #{s : (Listof Symbol)}) ...) s]
    [_ (error "expected only symbols, given" lst)]))

(ann (+ 7 1) Number)

(let ([x 7]) (add1 x))

(: sum-list (-> (Listof Number) Number))
(define (sum-list l)
  (cond [(null? l) 0]
        [else (+ (car l) (sum-list (cdr l)))]))

(struct None ())
(struct (a) Some ([v : a]))
 
(define-type (Opt a) (U None (Some a)))
 
(: find (-> Number (Listof Number) (Opt Number)))
(define (find v l)
  (cond [(null? l) (None)]
        [(= v (car l)) (Some v)]
        [else (find v (cdr l))]))

(: list-length (All (A) (-> (Listof A) Integer)))
(define (list-length l)
  (if (null? l)
      0
      (add1 (list-length (cdr l)))))

;在字符串之间插入"-"
(: link (-> (Listof String) String))
(define (link ls)
  (let loop ((l ls)
             (res ""))
    (if (eq? (car l) (last l))
        (string-append res (car l))
        (loop (cdr l) (string-append res (car l) "-")))))

;(link '("0" "1" "2"))

;(foldl (λ ([s : String] [r : String]) (string-append r "-" s)) "0" '("1" "2" "3"))

(: flexible-length (-> (U String (Listof Any)) Integer))
(define (flexible-length str-or-lst)
  (if (string? str-or-lst)
      (string-length str-or-lst)
      (length str-or-lst)))

(: strlen (-> String Number))
(define (strlen s)
    (string-length s))

(: big? (-> Real Boolean))
(define (big? n)
  (> n 150))
