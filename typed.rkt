#lang typed/racket

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

(: flexible-length (-> (U String (Listof Any)) Integer))
(define (flexible-length str-or-lst)
  (if (string? str-or-lst)
      (string-length str-or-lst)
      (length str-or-lst)))

(: square (-> Real Real))
(define (square n)
  (* n n))

(: assert-symbols! ((Listof Any) -> (Listof Symbol)))
(define (assert-symbols! lst)
  (match lst
    [(list (? symbol? #{s : (Listof Symbol)}) ...) s]
    [_ (error "expected only symbols, given" lst)]))

(define ls '("h" "e" "l" "l" "o"))

(: concat (-> (Listof String) String))
(define (concat ls)
  (foldl (λ ([s : String] [res : String]) (string-append res s)) "" ls))

(concat ls)

