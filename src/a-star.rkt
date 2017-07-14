#lang racket

;估值函数目前无法估算路线上存在障碍物的情况
;目的是做一个类智能解（最短路径之一，不是遍历所有路径），而非将一个类智能问题转换成一个搜索问题（从所有解中搜索最短路径解）

(define board
  (append (list '(0 0 X 0 X 0 0 X))
          (list '(0 X 0 X 0 X 0 X))
          (list '(0 0 0 X 0 X 0 X))
          (list '(0 X 0 0 X X 0 X))
          (list '(0 X 0 0 0 X X X))
          (list '(0 X X X X 0 0 0))
          (list '(0 0 0 0 0 0 X 0))
          (list '(0 X 0 0 X 0 0 X))))

;列表信息说明：(1 1 7 5 1.4142135623730951) 前两位是current-pos 第三位是相对前一点的位置 第四位是终点相对当前点的位置 第五位是当前点距终点的位置

;x:row y:col
(define (find-board pos)
  (let ((x (first pos))
        (y (second pos)))
    (list-ref (list-ref board x) y)))

;测试棋盘规模
(define (size board)
  (list (length board)
        (length (car board))))

;边界检测pos = '(x y)
(define (check pos)
  (let ((x (first pos))
        (y (second pos))
        (s (size board)))
    (and (>= x 0)(< x (first s))
         (>= y 0)(< y (second s)))))
        
;方向
(define (direction pos)
  (let ((x (first pos))
        (y (second pos)))
    (list (list (sub1 x) (sub1 y))    ;左上方方向0
          (list (sub1 x) y)           ;正上方方向1
          (list (sub1 x) (add1 y))    ;右上方方向2
          (list x (sub1 y))           ;左方方向3
          (list x (add1 y))           ;右方方向4
          (list (add1 x) (sub1 y))    ;左下方方向5
          (list (add1 x) y)           ;正下方方向6
          (list (add1 x) (add1 y))))) ;右下方方向7

;生成pos的下一步坐标集合
(define (gen-path pos)
  (let ((dirt (direction pos)))
    (filter (λ (x) (and (check x) x)) dirt)))

;消除pos下一步中存在障碍物的坐标
(define (!block pos)
  (filter-not (λ (p) (eq? 'X (find-board p))) (gen-path pos)))

;返回终点相对于当前坐标的方向，忽略障碍物
(define (end-direction pos end)
  (let ((px (first pos))
        (py (second pos))
        (ex (first end))
        (ey (second end)))        
    (cond ((= px ex) ;在方向3和4上
           (if (> (- ey py) 0)
               4   ;终点在当前点的右侧
               3)) ;终点在当前点的左侧
          ((= py ey) ;在方向1和6上
           (if (> (- ex px) 0)
               6
               1))
          ((< px ex)
           (if (> (- ey py) 0)
               7
               5))
          ((> px ex)
           (if (> (- ey py) 0)
               2
               0)))))

;距离 exp:(distance '(1 1) '(2 2))
(define (distance pos end)
  (let ((px (first pos))
        (py (second pos))
        (ex (first end))
        (ey (second end)))
    (cond ((= ex px) (abs (- ey py)))
          ((= ey py) (abs (- ex px)))
          (else 
           (sqrt (+ (abs (expt (- ex px) 2))
                    (abs (expt (- ey py) 2))))))))

;价值评估 next-pos-set = (!block pos)
;计算出当前可行路径相对终点的距离
(define (evaluate next-pos-set end)
  (map (λ (p) (append (list p) (list (distance p end)))) ;(end-direction p end))))
       next-pos-set))

;两个列表是否相等
(define (list=? ls lt)
  (cond ((null? ls) (null? lt))
        ((null? lt) (null? ls))
        ((eq? (car ls) (car lt))
         (list=? (cdr ls) (cdr lt)))
        (else #f)))

;将来路从可行路径中删除
;(!comeback '((2 2) (0 0)) '(((0 0) 2) ((1 2) 3)))
;(!comeback '((0 3)) (evaluate (!block '(1 2)) '(7 3)))
(define (!comeback come-pos next-pos-set)
  (remove* come-pos next-pos-set (λ (x y) (and (eq? (car x) (caar y))
                                               (eq? (cadr x) (cadar y))))))

;检查路径，如果距离值持续减小则意味着路径在逼近终点，如果距离值增加则意味着触碰了障碍，正在拐头，该函数返回增加开始和结尾处的两组坐标
;path:已生成的一条路径
(define (milestone path end)
  (let loop ((path (evaluate path end)))
    ;(if #t (call/cc (λ (k) (k path))) ;查看路径值
    (cond ((null? (cdr path)) '())
          ((> (- (second (cadr path)) (second (car path))) 0)
           (cons (cadr path) (loop (cdr path))))
          (else (loop (cdr path))))))

;联通路径最多的点
(define (cross path)
  (let ((len (map (λ (x) (length (!block x))) path))) ;list
    (let loop ((max-len (apply max len)) ;value
               (len len) ;list
               (path path)) ;list
      (cond ((null? path) '())
            ((= max-len (car len))
             (cons (car path)
                   (loop max-len (cdr len) (cdr path))))
            (else (loop max-len (cdr len) (cdr path)))))))
    

;探路，进行垂直和平行并且不走回原路的探测，如果有障碍则消除该条路径
#|(define (see-future curr-pos end)
  )|#

;求解路径 nps:next-pos-set
(define (find-path start end)
  (let loop ((nps (sort (evaluate (!block start) end)
                        < #:key (λ (x) (list-ref x 1))))        
             (res (list start))
             (rem '()))    
    (cond ((list=? end (caar nps))
           (reverse (cons end res)))
           ;rem)
          (else (loop (!comeback res
                                 (sort (evaluate (!block (caar nps)) end)
                                       < #:key (λ (x) (list-ref x 1))))
                      (cons (caar nps) res)
                      (cons (!comeback res
                                       (sort (evaluate (!block (caar nps)) end)
                                             < #:key (λ (x) (list-ref x 1))))
                            rem))))))

(define (find-path+ start end)
  (let ((nps (sort (evaluate (!block start) end) < #:key (λ (x) (list-ref x 1))))
        (res (list start)))
    (define (iter nps rem res cb path) ;cb目的是在一条路径失败后为防止新的尝试再次走上该路径，故将res暂时保存下来 path用于保存所有有效路径 flag用于标注rem为空的状态
      (cond ((member end nps (λ (x y) (and (eq? (car x) (caar y)) (eq? (cadr x) (cadar y)))))
             #|(if (null? (car rem))
                 (iter nps (cdr rem) (cdr res) cb path flag)
                 (iter (car rem) (cdr rem) (cdr res) cb (cons (reverse (cons end res)) path) #f)))|#             
             (reverse (cons end res)))
             ;rem)
             ;(iter (last rem) '() (list start) (list start) (cons (reverse (cons end res)) path)))
            ;((and (null? rem) (eq? #t flag)) path)
            ((null? nps) ;只有在路径不通的情况下
             #|(if (null? (car rem)) ;回退到上一层尝试
                 (iter nps (cdr rem) (cdr res) cb)
                 (iter (car rem) (cdr rem) (cdr res) cb)))|#
             (iter (last rem) '() (list start) cb path)) ;直接从起点重新尝试
             ;rem)
            (else (iter (!comeback cb (sort (evaluate (!block (caar nps)) end) < #:key (λ (x) (list-ref x 1))))                        
                        (cons (cdr nps) rem)
                        (cons (caar nps) res)
                        (cons (caar nps) res)
                        path
                        ))))
    (iter nps '() res res '())))

(define reuslt '())

(define (find-path++ start end)
  (let ((next (!block start)))
    (let loop ((ls next))
      (cond ((null? ls) '())
            (else (cons (cons start (find-path+ (car ls) end))
                        (loop (cdr ls))))))))


;一个节点的分支越多就越重要，现在因为不需要完全遍历，所以只用在重要节点处遍历就可




(find-path+ '(0 3) '(7 3))