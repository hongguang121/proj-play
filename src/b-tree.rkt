#lang racket

(require pict
         pict/tree-layout)

(require "new-sort.rkt")

(define (atom? x)
  (cond ((null? x) #f)
        ((pair? x) #f)
        (else #t)))

;brh = 'left/'right
(define (find-branch tree brh)
  (cond ((or (= (length tree) 1)
             (null? tree))
         '())
        ((eq? 'left brh) (cadr tree))
        ((eq? 'right brh) (caddr tree))
        (else (error "Not tree!"))))

(define (insert a tree sign)
  (let ((big "")
        (small ""))
    (cond ((eq? sign 'string)
           (begin
             (set! big string>?)
             (set! small string<=?)))
          ((eq? sign 'num)
           (begin
             (set! big >)
             (set! small <=))))
  (define (iter tr)
    (cond ((null? tr) (cons a (cons '() (cons '() '()))))
          ((small a (car tr))
           (cons (car tr)
                 (cons (iter (find-branch tr 'left))
                       (cons (find-branch tr 'right) '()))))
          ((big a (car tr))
           (cons (car tr)
                 (cons (find-branch tr 'left)
                       (cons (iter (find-branch tr 'right)) '()))))))
    (iter tree)))

;insert列表
(define (insert* ls tree sign)
  (cond ((null? ls) tree)
        (else (insert* (cdr ls) (insert (car ls) tree sign) sign))))

;关于删除操作：删除某节点势必会影响该节点后续节点的结构，所以删除节点后需要将剩余节点递归生成二叉树。

;将复合列表变为简单列表
;flatten
  
(define (delete a tree)
  (define (iter tr)
    (cond ((null? tr) '())
          ((< a (car tr))
           (cons (car tr)
                 (cons (iter (find-branch tr 'left))
                       (cons (find-branch tr 'right) '()))))
          ((> a (car tr))
           (cons (car tr)
                 (cons (find-branch tr 'left)
                       (cons (iter (find-branch tr 'right)) '()))))
          ((= a (car tr))           
           (cond ((null? (find-branch tr 'left)) (find-branch tr 'right))
                 ((null? (find-branch tr 'right)) (find-branch tr 'left))
                 (else 
                  (if (> (depth* (find-branch tr 'left))
                         (depth* (find-branch tr 'right)))
                      (insert* (flatten (find-branch tr 'right)) (find-branch tr 'left) 'num)
                      (insert* (flatten (find-branch tr 'left)) (find-branch tr 'right) 'num)))))))
  (iter tree))

(define (search a tree)
  (define (iter tr rem)
    (cond ((null? tr) '())
          ((< a (car tr))
           (iter (find-branch tr 'left)
                 (cons (car tr) rem)))
          ((> a (car tr))
           (iter (find-branch tr 'right)
                 (cons (car tr) rem)))
          (else (reverse (cons a rem)))))
  (iter tree '()))

;生成二叉树
;(bst '(54 31 29 45 25 38 21 18 17 55 64 9 72 12 81) 'num)
;(bst '("e" "g" "b" "d" "f") 'string)
;sign:'num 'string
(define (bst ls sign)
  (define (iter ts tr)
    (cond ((null? ts) tr)
          (else (iter (cdr ts)
                      (insert (car ts) tr sign)))))
  (iter ls '()))

;完全二叉树，符合二叉搜索树性质的二叉堆，是二叉搜索树中的特例

;(bst '(8 5 10 4 6 9 11) 'num)

;树深
(define depth*
  (lambda (l)
    (cond
      ((null? l) 1)
      ((atom? (car l))
       (depth* (cdr l)))
      (else
       (let ((a (+ 1 (depth* (car l))))
             (d (depth* (cdr l))))
         (cond
           ((> d a) d)
           (else a)))))))

;前序遍历
(define (iter-dlr btr)
  (cond ((null? btr) '())
        (else
         (append (list (car btr))
                 (iter-dlr (find-branch btr 'left))
                 (iter-dlr (find-branch btr 'right))))))

;中序遍历
(define (iter-ldr btr)
  (cond ((null? btr) '())
        (else
         (append (iter-ldr (find-branch btr 'left))
                 (list (car btr))
                 (iter-ldr (find-branch btr 'right))))))

;后序遍历
(define (iter-lrd btr)
  (cond ((null? btr) '())
        (else
         (append (iter-lrd (find-branch btr 'left))
                 (iter-lrd (find-branch btr 'right))
                 (list (car btr))))))

;层序遍历
(define (iter-flr tr)
  (define (floor tr)
    (flatten (cons (car tr)
                   (iter (cadr tr) (caddr tr)))))
  (define (iter left right)
    (cond ((and (null? left)(null? right)) '())
          ((null? left)
           (cons (car right)                                             
                 (iter (cadr right) (caddr right))))
          ((null? right)
           (cons (car left)                      
                 (iter (cadr left) (caddr left))))
          (else (cons (car left)
                      (cons (car right)
                            (cons (iter (cadr left) (caddr left))                            
                                  (iter (cadr right) (caddr right))))))))
  (floor tr))

;(define (iter-traverse btr)
 
(define (flatten* lst)
    (cond ((null? lst) '())          
          ((list? (car lst))
           (append (flatten (car lst))
                   (flatten (cdr lst))))
          (else (cons (car lst)
                      (flatten (cdr lst))))))

(define (select ls)
  (list
   (λ (n) (+ n (car ls)))
   (λ (x) (* x (car ls)))))
((car (select '(1 2 3))) 1)

(define (b-flr exp)  
  (match exp
    ((? number? x) x)
    ((? null? x) '())
    (`(,e1 ,e2 ,e3)
     (let ((v1 (b-flr e1))
           (v2 (b-flr e2))
           (v3 (b-flr e3)))
       (cons v1 (append v2 v3))))))

(define (flat-tr btr)
  (cond ((null? btr) '())
        ((not (pair? btr)) (write btr))
        (else (flat-tr (car btr))
              (flat-tr (cdr btr)))))

(define (sorted? lst cmp)
  (or (< (length lst) 2)
      (and (cmp (car lst)
                (cadr lst))
           (sorted? (cdr lst) cmp))))

(define (bar a b c . d)
    (list a b c d))


;search min/max
(define (min/max btree pos)
  (if (eq? pos 'min)
      (set! pos 'left)
      (set! pos 'right))
  (let loop ((tr (find-branch btree pos))
             (res '()))
    (if (null? tr)
        (car res)
        (loop (find-branch tr 'left) (cons (car tr) res)))))

;binary-tree
(define (entry tree)
  (if (null? tree)
      (error "node is nil")
      (car tree)))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (entry set)) #t)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

;平衡树
;首先列表必须排序，然后①找到列表中值(和二分查找相同)并将其作为根节点，由此②将列表分为左侧，根节点和右侧，
;之后对左右侧列表分别递归进行①②步骤，直到列表为空。
;如果要插入或删除元素，一种方案是将列表重排，然后(list->tree)，但是效率差，另一种方案是缩小排序范围，但需要
;检查各节点深度，如果出现不平衡的状态，就需要通过左右旋转调整
;exp:(list->tree (sort '(54 31 29 45 25 38 21 18 17 55 64 9 72 12 81) <))
(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

;计算node的balance-factor，值是-1,0,1就是平衡的
(define (balance-factor node)
  (- (depth* (find-branch node 'left))
     (depth* (find-branch node 'right))))

;check-every-node-balance-factor
(define (check tree)
  (cond ((null? tree) '())
        (else (cons (cons (balance-factor tree) (car tree))
                    (append (check (find-branch tree 'left))
                            (check (find-branch tree 'right)))))))

;找出树的一部分
;(find-avl 15 (insert-avl 15 (insert-avl 13 (insert-avl 10 (list->tree '(1 3 5 7 9 11))))))
(define (find-avl x set)
  (cond ((null? set) #f)
        ((= x (entry set)) set)
        ((< x (entry set))
         (find-avl x (find-branch set 'left)))
        ((> x (entry set))
         (find-avl x (find-branch set 'right)))))

;向avl-tree中插入元素
(define (insert-avl x set)
  (cond ((null? set) (make-tree x '() '()))        
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (insert-avl x (find-branch set 'left))
                    (find-branch set 'right)))
        ((> x (entry set))
         (make-tree (entry set)
                    (find-branch set 'left)
                    (insert-avl x (find-branch set 'right))))))

;如果x是list
(define (insert-lst x set)
  (cond ((null? x) set)
        ((not (pair? x)) (insert-avl x set))
        (else (insert-lst (cdr x) (insert-avl (car x) set)))))

;从avl-tree中删除元素
(define (del x set)
  (cond ((null? set) #f)
        ((= x (entry set))
         (cond ((and (eq? '() (cadr set)) ;删除的是叶子
                     (eq? '()  (caddr set))) '())
               (else (cons (car (find-branch set 'right))
                           (cons (insert-lst (flatten (find-branch (find-branch set 'right) 'left)) (find-branch set 'left))
                                 (cons (find-branch (find-branch set 'right) 'right)
                                       '()))))))
        ((< x (entry set))
         (make-tree (entry set)
                    (del x (find-branch set 'left))
                    (find-branch set 'right)))
        ((> x (entry set))
         (make-tree (entry set)
                    (find-branch set 'left)
                    (del x (find-branch set 'right))))))

;单左旋，左子树深度小于右子树时
(define (levorotation set)
  (let ((node (entry set))
        (left (find-branch set 'left))
        (right (if (not (null? (find-branch set 'right))) (find-branch set 'right) '(_))))
    (cons (car right)
          (cons (cons node (cons left (list (find-branch right 'left))))
                (cons (find-branch right 'right) '())))))

;单右旋，右子树深度小于左子树时
(define (dextrorotation set)
  (let ((node (entry set))
        (left (if (not (null? (find-branch set 'left))) (find-branch set 'left)  '(_)))
        (right (find-branch set 'right)))
    (cons (car left)
          (cons (find-branch left 'left)
                (cons (cons node (cons (find-branch left 'right) (list right))) '())))))

;左右双旋转
(define (double-ror-lr set)
  (let* ((k1 (find-branch set 'left))
         (k2 (list (entry set) (levorotation k1) (find-branch set 'right)))
         (k3 (dextrorotation k2)))
    k3))

;右左双旋转
(define (double-ror-rl set)
  (double-ror-lr (levorotation set)))

(define la (list->tree (sort '(54 31 29 45 25 38 21 18 17 55 64 9 72 12 81) <)))
(define lb '(9 (5 () ()) (15 (12 (10 () ()) (13 () ())) (17 () ())))) ;可以通过右左双旋转恢复平衡
;(egaku (find-avl 25 (del 18 la)))
;(egaku (double-ror-lr (find-avl 25 (del 18 la))))
;(egaku lb)
;(egaku (double-ror-rl lb))


;找到不平衡点，然后旋转 direction:levorotation/dextrororation
(define (unavl x set direction)
  (cond ((= x (entry set)) (direction set))
        ((< x (entry set))
         (make-tree (entry set)
                    (unavl x (find-branch set 'left) direction)
                    (find-branch set 'right)))
        ((> x (entry set))
         (make-tree (entry set)
                    (find-branch set 'left)
                    (unavl x (find-branch set 'right) direction)))))

;恢复平衡，应该通过旋转，即区分何时采用哪种旋转方式
(define (return-avl set)
  (list->tree (sort (flatten set) <)))

;将树画出
;node pic
(define (node-pic r num)
         (cc-superimpose (filled-ellipse r r #:color "white") (text (number->string num) (cons 'bold 'default) 12)))

;tree => (list tree)
(define (change tree)
  (cond ((null? tree) '())
        ((number? (car tree))
         (cons 'tree-layout
               (cons '#:pict
                     (cons (node-pic 25 (car tree))
                           (change (cdr tree))))))
        ((or (eq? (car tree) '_) ;可以不需要
             (eq? (car tree) '()))
             (change (cdr tree)))
        (else (cons (change (car tree))
                    (change (cdr tree))))))

;(egaku la)
(define (egaku tree)
  (let ((res (change (list tree))))
    (eval (append '(naive-layered) res))))

(define ls '(6 (2 (1 () ()) (5 (3 () (4 () ())) ())) (8 () ()))) ;二叉树
(define lt '(1 () (2 () (3 () ()))))
(define ltt '(3 (2 (1 () ()) ()) ()))
(define lts '(2 (1 () ()) (4 (3 () ()) (5 () (6 () ()))))) ;二叉树
;(egaku lt)
;(egaku (levorotation lt))
;(egaku (dextrororation ltt))
;(egaku (levorotation lts))
;(egaku (insert 7 ls 'num))

;二叉堆
(define exp-heap '(1 (2 (4 () ()) (5 () ())) (3 (6 () ()) (7 () ()))))

;首先创建一个二叉堆 ;ls = '(54 31 29 45 25 38 21 18 17 55 64 9 72 12 81)
;要求必须是完全二叉树，且每个节点的子节点值均要大于该节点
(define (b-heap ls)
  (let ((id 0))
    (map (λ (x) (set! id (add1 id)) (cons id x)) (sort ls <))))

;通过下标查找堆中元素
(define (find-by-id bh id) (cdr (assq id bh)))

;通过元素查找下标
(define (find-by-item bh item)
  (cond ((null? bh) "not find id")
        ((= (cdar bh) item) (caar bh))
        (else (find-by-item (cdr bh) item))))

;通过id交换位置 a,b是元素
(define (change-item bh a b)
  (map (λ (x)
         (cond ((= a (cdr x)) (cons (car x) b))
               ((= b (cdr x)) (cons (car x) a))
               (else x)))
       bh))

;insert
(define (insert-heap bh item)
  (let ((bh (append bh (list (cons (add1 (length bh)) item)))))
    (let ((id (find-by-item bh item)))
      (let loop ((father (floor (/ id 2))))
        (cond ((= father 0) bh)
              ((> (find-by-id bh father) item)
               (begin
                 (set! bh (change-item bh (find-by-id bh father) item))
                 (loop (floor (/ father 2)))))
              (else bh))))))

;向二叉堆插入元素 广度优先
;首先生成堆深度相应的检查路径，如果树很深，那么路径会非常多，可以考虑通过延迟求取路径提升性能，
;即先检查一部分路径，如果符合条件就直接返回，不符合条件的话再接着生成新路径，然后检查
(define (gen-path depth)
  (let gen ((c (sub1 depth))
                 (path '()))
    (if (or (= c 0)(= c 1))
        (apply cartesian-product path)
        (gen (sub1 c) (cons '(left right) path)))))

;检查一条路径是否可以插入节点
(define (check-heap heap path)  
  (cond ((null? heap) #t)
        ((null? path) #f)
        (else (check-heap (find-branch heap (car path)) (cdr path)))))

;按从左到右的顺序检查所有路径，当有一条路径为真则立即返回
;(check-all exp-heap (gen-path (depth* exp-heap)))
(define (check-all heap path+)
  (cond ((null? path+) (check-all heap (gen-path (add1 (depth* heap)))))
        ((eq? (check-heap heap (car path+)) #t) (car path+))
        (else (check-all heap (cdr path+)))))

;向二叉堆中插入新节点
(define (inst-heap a heap)
  (let loop ((heap heap)
             (path (check-all heap (gen-path (depth* heap)))))    
      (cond ((null? heap) (make-tree a '() '()))
            ((eq? 'left (car path))
             (make-tree (car heap)
                        (loop (find-branch heap 'left) (cdr path))
                        (find-branch heap 'right)))
            ((eq? 'right (car path))
             (make-tree (car heap)
                        (find-branch heap 'left)
                        (loop (find-branch heap 'right) (cdr path)))))))

;向二叉堆中插入一组节点
;(egaku (cons-heap (range 1 13) '()))
(define (cons-heap ls heap)
  (cond ((null? ls) heap)
        (else (cons-heap (cdr ls) (inst-heap (car ls) heap)))))

;delete


;栈

;treap 树堆:同时具备二叉查找树和堆的性质；①



;红黑树



;Huffman tree
(struct leaf (symbol weight))
;leaf? leaf-symbol leaf-weight

