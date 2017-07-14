;;在操作列表时请分清何时使用列表的值（car）进行操作
;;以及何时使用列表的下标（引用）进行操作
;;如果实现或想法在逻辑上过于复杂，一定要跳出来重新审视，缺乏简洁性的程序必定存在问题
;;lambda的嵌套规则：1.单参数时((lambda (x) (* x x))9)同define相同,即：(lambda定义体)(传入参数)
;;2.多参数时(((lambda (x) (lambda (y) (* y y)))9)5) ;25/((lambda (x) ((lambda (y) (* y y)) 9))5) ;81/注意：((lambda (x) (lambda (y) (* y y)) 9)5) ;9
;;可以是(lambda定义体)(参数1)(参数2).../也可以是((lambda定义体((lambda定义体)(参数1)))(参数2))...
;;要注意((lambda (x) ((lambda (y) (* y y)) 9)5)4)/((lambda (n) (list 1))2)
;;(letrec ((iter (lambda (args...)))) (iter args...))定义了相当于内层函数((define (iter (args...))) (iter args...))
;;递归决定最终返回值
;;Lexical scoping:((lambda (y) (((lambda (y) (lambda (x) (* y 2))) 3) 0)) 4) ;6
;;为了避免穷举，可以选择模式匹配

(require racket/future)
(require racket/file)
(require htdp/matrix)


(define % remainder)

;(file->list "calc.rkt")
;(file->lines "calc.rkt")
;(make-directory "e:/program/src/test")
;(current-directory)
;(string->list "String")
;(list->string '(#\S #\t #\r #\i #\n #\g))
;(regexp-split #rx"" "smithereens")
;(list->string (reverse (string->list "***")))

(define atom? 
  (lambda (e)
    (if (and (not (pair? e)) (not (null? e)))
        #t
        #f)))

(define lat?
  (lambda (l)
    (cond ((null? l) #t)
          ((atom? (car l)) (lat? (cdr l)))
          (else #f))))

(define member?
  (lambda (a lat)
    (cond ((null? lat) #f)
          (else (or (eq? a (car lat))
                    (member? a (cdr lat)))))))

(define div
  (lambda (n m)
    (if (zero? m) 
        'error
        (cond ((< n m) 0)
              (else (add1 (div (- n m) m)))))))
(define div+
  (lambda (n m)
    (if (= (remainder n m) 0)
        (/ n m)
        (div+ (- n (remainder n m)) m))))

;;sort algorithms
(define (last l)
  (define (last-iter s n)
    (if (= n 1)
        (car s)
        (last-iter (cdr s) (- n 1))))
  (last-iter l (length l)))

(define (no-last l)
  (define (iter s n)
    (if (= n 0)
        '()
        (cons (car s) 
              (iter (cdr s) (- n 1)))))
  (iter l (- (length l) 1)))

(define (len item)
  (define (length-iter item count)
    (if (null? item)
        count
        (length-iter (cdr item) (+ count 1))))
  (length-iter item 0))

;;quick-sort
(define (q-sort s)
  (if (< (length s) 2)
      s
      (append
       (q-sort (filter 
                (lambda (x)
                  (< x (last s)))
                s))
       (filter (lambda (x)
                 (= x (last s)))
               s)
       (q-sort (filter 
                (lambda (x)
                  (> x (last s)))
                s)))))

(define qsort
  (lambda (s)
    (if (null? s) '()
        (let ((pivot (find s (div (len s) 2))))
          (append (qsort (filter (lambda (x) (< x pivot)) s))
                  (filter (lambda (x) (= x pivot)) s)
                  (qsort (filter (lambda (x) (> x pivot)) s)))))))

;;remove member
(define rember
  (lambda (a lat)
    (cond ((null? lat) '())
          (else (cond ((eq? a (car lat)) (cdr lat))
                      (else (cons (car lat)
                                  (rember a (cdr lat)))))))))
;;bubble-sort
(define (bubble-sort s)
  (define (iter l result)
    (if (null? l)
        result
        (iter (rember (apply max l) l)
              (cons (apply max l) result))))
  (iter s '()))

;;merge-sort
(define (cut str start end)
  (define (iter add count s r)
    (cond ((> start end) 
           (if (not (< add end))
               (if (not (< count (+ (- start end) 1)))
                   r
                   (iter add (+ count 1) (cdr s) 
                         (cons (car s) r)))
               (iter (+ add 1) count (cdr s) r)))
          ((not (> start end)) 
           (if (not (< add start))
               (if (not (< count (+ (- end start) 1)))
                   '()
                   (cons (car s)
                         (iter add (+ count 1) (cdr s) r)))
               (iter (+ add 1) count (cdr s) r)))))
  (iter 0 0 str '()))

(define (merge left right)
  (cond ((null? left) right)
        ((null? right) left)
        (else 
         (let ((l (car left)) (r (car right)))
           (cond ((< l r) (cons l 
                                (merge (cdr left) right)))
                 ((> l r) (cons r
                                (merge left (cdr right))))
                 ((= l r) (cons l
                                (merge (cdr left) right))))))))

(define (merge-sort s)
  (if (= (length s) 1)
      s
      (let* ((mid (div (length s) 2))
             (left (cut s 0 (- mid 1)))
             (right (cut s mid (- (length s) 1))))
        (merge (merge-sort left) 
               (merge-sort right)))))

(define (swap lst i j)
  (define (find arr n)
    (if (= n 0)
        (car arr)
        (find (cdr arr) (- n 1))))
  (define (swap-iter arr count)
    (cond ((= count i) (cons (find lst j) (swap-iter (cdr arr) (+ count 1))))
          ((= count j) (cons (find lst i) (swap-iter (cdr arr) (+ count 1))))
          ((null? arr) '())
          (else (cons (car arr) (swap-iter (cdr arr) (+ count 1))))))
  (swap-iter lst 0))

(define (find-add lst n)
  (define (find-add-iter arr count)
    (if (null? arr) 
        '()
        (if (= (- count 1) n)
            (cons 'D 
                   (find-add-iter arr (+ count 1)))
            (cons (car arr) 
                  (find-add-iter (cdr arr) (+ count 1))))))
(find-add-iter lst 0))

;;insert-sort
;;将元素下标obj插入至目标元素下标aim的左侧
(define (insertL s aim obj)
  (define (iter l id)
    (cond ((null? l) '())
          ((= aim obj id)
           (cons (find s aim) (cdr l)))
          ((and (eq? (car l) (find s aim)) (= aim id))
           (iter (rember (car l) l) (+ id 1)))
          ((and (eq? (car l) (find s obj)) (= obj id))
           (cons (find s aim)
                 (cons (car l)
                       (iter (rember (car l) l) (+ id 1)))))
          (else (cons (car l)
                      (iter (cdr l) (+ id 1))))))
  (iter s 0))

(define (find arr n)
  (cond ((or (< n 0) (not(< n (length arr)))) '())
        ((= n 0) (car arr))
        (else (find (cdr arr) (- n 1)))))

(define (insert-sort s)
  (define (iter l i n)
    (cond ((= i (- (length l) 1)) l)
          ((not (< n 0))
           (cond ((> (find l n) (find l (+ n 1)))
                  (iter (insertL l (+ n 1) n) i (- n 1)))
                 (else (iter l (+ i 1) (+ i 1)))))
          (else (iter l (+ i 1) (+ i 1)))))
  (iter s 0 0))

;;(define s '(8 12 10 1 2 5 15 18 16 21 25 17 9 4 5 7 6 3))
;;(define c '(5 5 4 6 5 6 6 4 5 6))

(define (power a b)
  (if (< b 0) 
      (/ 1 (power-iter 1 a b))
      (power-iter 1 a b)))

(define (power-iter n a b)
  (if (< (abs b) 1)
      n
      (power-iter (* n a) a (- (abs b) 1))))

;;shell-sort
(define (copy s)
  (cond ((null? s) '())
        ((atom? (car s))
         (cons (car s)
               (copy (cdr s))))
        (else (cons (copy (car s))
                    (copy (cdr s))))))

;;通过下标序列dest在元素序列meta中找到对应值
(define (find* meta dest)
  (if (null? dest)
      '()
      (cons (find meta (car dest))
            (find* meta (cdr dest)))))

;;以步进increment生成下标序列
(define (index ls increment)
  (define (iter inc)
    (if (> (+ inc increment) (- (length ls) 1))
        '()
        (cons (+ inc increment)
              (iter (+ inc increment)))))
  (iter (- increment)))

;;生成以inc为组数的序列s的下标
(define (generate ls inc)
  (define (iter l count)
    (cond ((null? l) '()) 
          ((= inc 1) 
           (cons (list (car l))
                 (iter (cdr l) count)))
          ((= count inc) '())
          ((= (last l) (- (length ls) 1))
           (cons l
                 (iter (map (lambda (x) (+ x 1)) (no-last l)) (+ count 1))))
          (else (cons l
                      (iter (map (lambda (x) (+ x 1)) l) (+ count 1))))))
  (iter (index ls inc) 0))

(define (sedgewick n)
  (define (iter i s)
    (if (= i n)
        (sort (filter (lambda (x) (> x 0)) s) >)
        (iter (+ i 1)
              (append s (list (+ (- (* 9 (power 4 i)) (* 9 (power 2 i))) 1)
                              (+ (- (power 4 i) (* 3 (power 2 i))) 1))))))
  (iter 0 '()))

(define (shell-sort s)
  (let ((x 5))
    (define (iter l m r n)
      (cond ((= n 1) (insert-sort l))
            ((null? m) (iter r (generate r (- n 2)) '() (- n 2)))
            (else (iter l (cdr m) (append r (insert-sort (find* l (car m)))) n))))
    (iter s (generate s x) '() x)))

;;生成以i为层数的n叉树的每层节点数目
(define (increment n i)
  (define (iter k)
    (if (= k i)
        '()
        (cons (power n k)
              (iter (+ k 1)))))
  (iter 0))

;;返回将s表转换成n叉树时的层数以及最后一层的叶子数
(define (npi+l n s)
  (define (iter sum i)
    (if (not (< sum (length s)))
        (list i (- (length s) (- sum (power n (- i 1)))))
        (iter (+ (power n i) sum) (+ i 1))))
  (iter 0 0))

(define div
  (lambda (n m)
    (if (zero? m) 
        'error
        (cond ((< n m) 0)
              (else (add1 (div (- n m) m)))))))

;;十进制转换成二进制
(define (to2 n)
  (define (iter n r)
    (cond ((= n 0) (append '(0) (list r)))
          ((= n 1) (append '(1) (list r)))
          (else (append (iter (div n 2) (remainder n 2)) (list r)))))
  (iter (div n 2) (remainder n 2)))

(define (to10 sn)
  (define (iter sn sum i)
    (cond ((null? sn) sum)
          ((= (car sn) 1) 
           (iter (cdr sn) (+ (power 2 i) sum) (- i 1)))
          ((= (car sn) 0) 
           (iter (cdr sn) sum (- i 1)))))
  (iter sn 0 (- (length sn) 1)))

;;生成n叉树的节点下标
(define (generate* ls)
  (define (iter node count index leaf s r t)
    (cond ((> count 0) 
           (iter (+ node 1) (- count 1) index leaf s (append r (list node)) t))
          ((and (= count 0) (< index (length s)))
           (iter node (find s index) (+ index 1) leaf s '() (append t (list r))))
          ((and (= count 0) (= index (length s)))
           (iter node count (+ index 1) leaf s '() (append t (list r))))
          ((and (> leaf 0) (> index (length s)))
           (iter (+ node 1) count index (- leaf 1) s (append r (list node)) t))
          (else (append t (list r)))))
  (iter 0 
        1 
        1
        (cadr (npi+l 2 ls)) 
        (increment 2 (- (car (npi+l 2 ls)) 1))
        '()
        '()))

;;找到一棵树的所有节点的值：(find-all s (generate* s))
(define (find-all s l)
  (cond ((null? l) '())
        (else (cons (find* s (car l))
                    (find-all s (cdr l))))))

;;遍历树：(fringe (find-all s (generate* s)))
(define (fringe tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (list tree))
        (else (append (fringe (car tree))
                      (fringe (cdr tree))))))

;;找出空节点（叶子）未完成
(define (leaf tr)
  (define (iter tree node)
    (cond ((null? tree) '())
          ((null? node) '())
          ((pair? node)
           )
          
          ((null? (car (cdr tr))) (cons node (cdr tree)))
          ((null? (cdr (cdr tr))) (cons (car (cdr tr)) (cdr tree)))
          (else (iter (cdr tree) (car tree) (car (cdr tree)) (car (cdr (cdr tree)))))))
  (iter tr (car tr)))

;;生成与l表等长的空表
(define (null-list l)
  (define (iter i)
    (if (= i (length l))
        '()
        (cons 'nil (iter (+ i 1)))))
  (iter 0))

;;计数排序核心
(define (count l)
  (define (iter s)
    (if (null? s)
        '()
        (cons (length (filter (lambda (x) (< x (car s))) l))
              (iter (cdr s)))))
  (iter l))

;;找出元素在表中的下标
(define (find-index ls num)
  (define (iter l count)
    (cond ((null? l) 'none)
          ((= (car l) num) count)
          (else (iter (cdr l) (+ count 1)))))
  (iter ls 0))

(define (insertR s aim obj)
  (define (iter l id)
    (cond ((null? l) '())
          ((= aim obj id)
           (cons (find s aim) (cdr l)))
          ((and (eq? (car l) (find s aim)) (= aim id))
           (iter (rember (car l) l) (+ id 1)))
          ((and (eq? (car l) (find s obj)) (= obj id))
           (cons (car l)
                 (cons (find s aim)
                       (iter (rember (car l) l) (+ id 1)))))
          (else (cons (car l)
                      (iter (cdr l) (+ id 1))))))
  (iter s 0))

;;将元素插入到下标处
(define (insert l item index)
  (define (iter s cnt)
    (cond ((null? s) '())
          ((= cnt index)
           (cons item (cdr s)))
          (else (cons (car s)
                      (iter (cdr s)(+ cnt 1))))))
  (iter l 0))

;;count-sort
(define (count-sort l)
  (define (iter s alter index)
    (cond ((null? index) alter)
          (else (iter (cdr s)
                      (insert alter (car s) (car index)) 
                      (cdr index)))))
  (iter l (null-list l) (count l)))

;;select-sort
(define (select-sort s)
  (define (iter l)
    (if (null? l)
        '()
        (cons (apply min l)
              (iter (rember (apply min l) l)))))
  (iter s))

;;生成一组随机序列
(define rand-lst 
  (lambda (n)
    (if (= n 0)
        '()
        (cons (random 1000)
              (rand-lst (- n 1))))))

;;生成随机数量r和随机位数s的一组序列
(define (rand r s)
  (define (iter r s)
    (if (= r 0)
        '()
        (cons (random s)
              (iter (- r 1) s))))
  (iter (random r) s))

(define (fib n)
  (define (iter f-1 f-2 cnt)
    (if (= cnt n)
       '()
       (cons f-1 
              (iter f-2 (+ f-1 f-2) (+ cnt 1)))));)
  (iter 0 1 0))

(define (fact n)
  (define (iter fn fm cnt)
    (if (= cnt n)
        fn
        (iter (* fn fm) (+ fm 1) (+ cnt 1))))
  (iter 1 1 0))

(define fact-cps
  (lambda (n ret)
    (let ((break ret))
      (let f ((n n)
              (ret ret))
        (cond ((= n 0)
               (ret 1))
              ((= n -1)
               (break -1))
              (else (f (- n 1)
                       (lambda (x)
                         (ret (* n x))))))))))

;取底
(define (int-f n)
  (define (iter c)
    (if (> c n)
        (- c 1)
        (iter (+ c 1))))
  (iter 0))
;取顶
(define (int-c n)
  (define (iter c)
    (if (not (< c n))
        c
        (iter (+ c 1))))
  (iter 0))

(define (dv-real r)
  (cons (int-f r) (list (- r (int-f r)))))

(define (gcd+ m n)
  (if (= n 0)
      m
      (gcd n (remainder m n))))

;(current-seconds)
;(current-inexact-milliseconds)

(define time current-inexact-milliseconds)

(define (prime? n)
  (define (iter p)
    (cond ((= (remainder n p) 0) #f)
          ((> p (sqrt n)) n)                
          (else (iter (+ p 1)))))
  (iter 2))

(define (fermat-test n)
  (define (iter a cnt)
    (cond ((and (> cnt 2)
                (= (remainder (power a n) n)
                   (remainder a n))) #t)
          ((= (remainder (power a n) n)
              (remainder a n))
           (iter (random (- n 1)) (+ cnt 1)))
          (else #f)))
  (iter (random (- n 1)) 0))

(define orf
  (lambda (ls)
    (cond
      [(null? ls) #f]
      [else 
       (let ([v (car ls)])
         (if (not (eq? v #f))
             v
             (orf (cdr ls))))])))

;从大到小取整数的各位数
(define (divs n)
  (define (iter i cnt bit)
    (if (> bit n)
        (if (= i cnt)
            '()
            (cons (% (div n (power 10 i)) 10)
                  (iter (+ i 1) cnt bit)))
        (iter i (+ cnt 1) (* bit 10))))
  (iter 0 1 10))

;将不足m的位补零
(define (add-zero n m)
  (define (iter i bit)
    (if (> bit n)
        (if (= i m)
            '()
            (cons (% (div n (power 10 i)) 10)
                  (iter (+ i 1) bit)))
        (iter i (* bit 10))))
  (iter 0 10))

(define (gen s)
  (if (null? s)
      '()
      (cons (add-zero (car s) 3)
            (gen (cdr s)))))

(define (gens s n)
  (map (lambda (x) (cons (find x n) (list x))) s))

(define (r-gens s)
  (map (lambda (x) (cadr x)) s))

(define (gen-id s)
  (define (iter id l)
    (if (null? l)
        '()
        (cons (list id (car l))
              (iter (+ id 1) (cdr l)))))
  (iter 0 s))

;多重序列插入排序
(define (i-sort s)
  (define (iter l i n)
    (cond ((= i (- (length l) 1)) l)
          ((not (< n 0))
           (cond ((> (car (find l n)) (car (find l (+ n 1))))
                  (iter (insertL l (+ n 1) n) i (- n 1)))
                 (else (iter l (+ i 1) (+ i 1)))))
          (else (iter l (+ i 1) (+ i 1)))))
  (iter s 0 0))

(define (to-int s)
  (define (iter l i cnt)
    (cond ((= cnt (length s)) '())
          ((null? l) 0)
          ((atom? (car l)) 
           (+ (* (car l) (power 10 i))
              (iter (cdr l) (+ i 1) cnt)))
          (else (cons (iter (car l) i cnt)
                      (iter (cdr l) i (+ cnt 1))))))
  (iter s 0 0)) 

(define (radix-sort ls)
  (define (iter s n b)
    (if (= n b)
        (to-int s)
        (iter (r-gens (i-sort (gens s n))) (+ n 1) b)))
  (iter (gen ls) 0 3))

(define s (rand-lst 100))

(define (bi-search ls n)
  (define (iter l r m)
    (cond ((or (< n (find ls l)) (> n (find ls r))) "none")
          ((eq? n (find ls m)) (find-index ls n))
          ((> n (find ls m)) (iter (+ m 1) r (div (+ (+ m 1) r) 2)))
          ((< n (find ls m)) (iter l (- m 1) (div (+ l (- m 1)) 2))))) 
  (iter 0 (- (length ls) 1) (div (length ls) 2)))

;双向链表,取值(map (lambda (x) (car x)) (linked s))
;查找id的前一项(find (linked s) (cadr (find (linked s) id)))
(define (linked ld)
  (define (iter prev id next)
    (cond ((null? (find ld prev)) 
           (cons (list (find ld id) 'nil id next)
                 (iter (+ prev 1) (+ id 1) (+ next 1))))
          ((null? (find ld next)) 
           (cons (list (find ld id) prev id 'nil) '()))
          (else (cons (list (find ld id) prev id next)
                      (iter (+ prev 1) (+ id 1) (+ next 1))))))
  (iter -1 0 1))

(define ret linked)

(define (id x ret)
  (ret x))

;https://projecteuler.net/problem=14
(define (collatz n)
  (define (iter i l)
    (cond ((= i 1) (reverse l))
          ((even? i) (iter (/ i 2) (cons (/ i 2) l)))
          ((odd? i) (iter (+ (* 3 i) 1) (cons (+ (* 3 i) 1) l)))))
  (iter n (list n)))

;找出以n为界的最长序列
(define (findmax n)
  (define (iter i max l)
    (if (> i n)
        l
        (if (> (len (collatz i)) max)
            (iter (+ i 1) (len (collatz i)) (list (len (collatz i)) i))
            (iter (+ i 1) max l))))
  (iter 1 1 '()))

;三角形数
(define (tri n)
  (define (iter i)
    (if (> i n)
        '()
        (cons (/ (* i (+ i 1)) 2) (iter (+ i 1)))))
  (iter 1))

;第n个三角形数
(define (tri-id n)
  (define (iter i)
    (if (= i n)
        (/ (* i (+ i 1)) 2)
        (iter (+ i 1))))
  (iter 0))

;自然数n的所有因子
(define (factor n)
  (define (iter i)
    (cond ((= n i) (cons i '()))
          ((= 0 (% n i)) (cons i (iter (+ i 1))))
          (else (iter (+ i 1)))))
  (iter 1))

(define (find-tri n)
  (define (iter i)
    (if (> (length (factor (tri-id i))) n)
        i
        (iter (+ i 1))))
  (iter 1))

(define union
  (lambda (set1 set2)
    (cond ((null? set1) set2)
          ((member? (car set1) set2)
           (union (cdr set1) set2))
          (else (cons (car set1)
                      (union (cdr set1) set2))))))

(define (sum-n inc m)
  (define (iter i)
    (if (not (< i m))
        '()
        (cons i (iter (+ inc i)))))
  (iter 0))

;生成空matrix
;也可以使用(list (list ~) (list ~) ...)
(define (gen-matrix row col)
  (define (iter cr cc s)
    (cond ((= cr row) '())
          ((= cc col)
           (cons s
                 (iter (+ cr 1) 0 '())))
          (else (iter cr (+ cc 1) (cons 0 s)))))
  (iter 0 0 '()))

;修改已生成的matrix的值
(define (alt-matrix gm row col val)
  (define (iter cr cc s l k)
    (cond ((> cr row) (cons k s))
          ((= cr row)
           (cond ((null? l) (iter (+ cr 1) cc s l (reverse k)))
                 ((= cc col) (iter cr (+ cc 1) s (cdr l) (cons val k)))
                 (else (iter cr (+ cc 1) s (cdr l) (cons (car l) k)))))
          (else (cons l
                      (iter (+ cr 1) cc (cdr s) (car s) k)))))
  (iter 1 1 (cdr gm) (car gm) '()))

(define (sum-of-prefixes tup)
  (define (iter sum tp)
    (cond ((null? tp) (list sum))
          (else (cons sum
                      (iter (+ (car tp) sum) (cdr tp)))))) 
  (iter (car tup) (cdr tup)))

(define sum-of-prefixes-1
  (lambda (tup)
    (letrec 
        ((I (lambda (sum tp)
              (cond ((null? tp) (list sum))
                    (else (cons sum
                                (I (+ (car tp) sum) (cdr tp))))))))
      (I (car tup) (cdr tup)))))

(define rember-beyond-first 
  (lambda (a lat)
    (letrec 
        ((R (lambda (lat)
              (cond ((null? lat) '())
                    ((eq? (car lat) a) '())
                    (else (cons (car lat)
                                (R (cdr lat))))))))
      (R lat))))

(define rember-upto-last 
  (lambda (a lat)
    (letrec 
        ((R (lambda (l)
              (cond ((null? l) lat)
                    ((eq? (car l) a) (cdr l))
                    (else (R (cdr l)))))))
      (R lat))))

(define rember-upto-last-cc
  (lambda (a lat)
    (call/cc 
     (lambda (skip)
       (letrec 
           ((R (lambda (lat)
                 (cond ((null? lat) '())
                       ((eq? (car lat) a) 
                        (skip (R (cdr lat))))
                       (else (cons (car lat)
                                   (R (cdr lat))))))))
         (R lat))))))

(define rm
  (lambda (a l)
    (call/cc 
     (lambda (oh)
       (letrec
           ((iter (lambda (l)
                    (cond ((null? l) oh '(no))
                          ((atom? (car l))
                           (if (eq? (car l) a)
                               (oh (iter (cdr l)))
                               (cons (car l)
                                     (iter (cdr l)))))))))
         (iter l))))))

(define Y
  (lambda (h)
    ((lambda (f) 
       (f f))
     (lambda (f)
       (h (lambda (x) ((f f) x)))))))

(define f
  (lambda (x)
    (/ (sin x) x)))

(define limit
  (lambda (f x)
    (if (= x 0)
        '()
        (cons (f x)
              (limit f (- x 1))))))

;笛卡尔积
(define (cartesian-product u v)
  (define (iter u l)
    (cond ((null? u) '())
          ((null? l)
           (iter (cdr u) v))
          (else (cons (list (car u) (car l))
                      (iter u (cdr l))))))
  (iter u v))

(define reciprocals
  (lambda (ls cc)
    (let ((break cc))
      (let f ((ls ls)
              (cc cc))
        (cond ((null? ls) (cc '()))
              ((= (car ls) 0)
               (break "zero found"))
              (else (f (cdr ls)
                       (lambda (x)
                         (cc (cons (/ 1 (car ls))
                                   x))))))))))

