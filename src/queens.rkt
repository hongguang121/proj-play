;8 queens
(define same?
  (lambda (l r)
    (cond ((and (null? l)(null? r)) #t)
          ((or (null? l)(null? r) #f))
          ((eq? (car l) (car r))
           (same? (cdr l) (cdr r)))
          (else #f))))

(define same*
  (lambda (l r)
    (cond ((and (null? l)(null? r)) #t)
          ((or (null? l)(null? r) #f))
          ((not (pair? (car l)))
           (if (eq? (car l) (car r))
               (same* (cdr l) (cdr r))
               #f))
          (else (cons (same* (car l) (car r))
                      (same* (cdr l) (cdr r)))))))

(define eqlist?
  (lambda (ls1 ls2)
    (cond ((null? ls1)(null? ls2))
          ((null? ls2) #f)
          (else (and (eq? (car ls1)(car ls2))
                     (eqlist? (cdr ls1)(cdr ls2)))))))

(define gen-map
  (lambda (i j)
    (letrec
        ((M (lambda (m n)
              (cond ((= m i) '())
                    ((= n j)
                     (M (+ m 1) 0))
                    (else (cons (list (list 'key: m n) 'value)
                                (M m (+ n 1))))))))
      (M 0 0))))

(define set-map
  (lambda (gm i j val)
    (letrec
        ((M (lambda (gm m n)
              (cond ((= n 8)
                     (M gm (+ m 1) 0))
                    ((and (= m i)(= n j))
                     (cons (list (list 'key: m n) val)
                           (cdr gm)))
                    (else (cons (car gm)
                                (M (cdr gm) m (+ n 1))))))))
      (M gm 0 0))))

;只用D2,D4,D6,D8即可
(define (set-D1 i j)
  (define (iter row col)
    (cond ((and (>= row 0)(>= col 0))
           (cons (list row col)
                 (iter (- row 1) (- col 1))))
          (else '())))
  (iter (- i 1) (- j 1)))

(define (set-D2 i j)
  (define (iter row col)
    (cond ((< row 8)
           (cons (list row col)
                 (iter (+ row 1) col)))
          (else '())))
  (iter 0 j))

(define (set-D3 i j)
  (define (iter row col)
    (cond ((and (>= row 0)(< col 8))
           (cons (list row col)
                 (iter (- row 1) (+ col 1))))
          (else '())))
  (iter (- i 1) (+ j 1)))

(define (set-D4 i j)
  (define (iter row col)
    (cond ((< col 8)
           (cons (list row col)
                 (iter row (+ col 1))))
          (else '())))
  (iter i 0))

(define (set-D5 i j)
  (define (iter row col)
    (cond ((< col 8)
           (cons (list row col)
                 (iter row (+ col 1))))
          (else '())))
  (iter i (+ j 1)))

(define (set-D6 i j)
  (define (iter row col)
    (cond ((and (< row 8)(>= col 0))
           (cons (list row col)
                 (iter (+ row 1) (- col 1))))
          (else '())))
  (iter (+ i 1) (- j 1)))

(define (set-D7 i j)
  (define (iter row col)
    (cond ((and (< row 8))
           (cons (list row col)
                 (iter (+ row 1) col)))
          (else '())))
  (iter (+ i 1) j))

(define (set-D8 i j)
  (define (iter row col)
    (cond ((and (< row 8)(< col 8))
           (cons (list row col)
                 (iter (+ row 1) (+ col 1))))
          (else '())))
  (iter (+ i 1) (+ j 1)))

(define (unsafe lat)
  (let ((i (car lat))
        (j (car (cdr lat))))
    (append (list lat)
            
            (set-D2 i j)
            
            (set-D4 i j)
            
            (set-D6 i j)
            
            (set-D8 i j))))

(define (safe? lst unsafe)
  (cond ((null? unsafe) #t)
        ((same? (car unsafe) lst) #f)
        (else (safe? lst (cdr unsafe)))))

;lat:(unsafe lst)
(define (safe lat row)
  (define (iter row col)
    (cond ((= col 8)
           '())
          ((safe? (list row col) lat)
           (cons (list row col)
                 (iter row (+ col 1))))
          (else (iter row (+ col 1)))))
  (iter row 0))

;组成ls1+ls2的集合
(define (yui ls1 ls2)
  (cond ((null? ls1) ls2)
        ((member (car ls1) ls2)
         (yui (cdr ls1) ls2))
        (else (cons (car ls1)
                    (yui (cdr ls1) ls2)))))

;ls1:列表 ls2:列表集合
(define (rem ls1 ls2)
  (cond ((null? ls2) '())
        ((same? ls1 (car ls2))
         (cdr ls2))
        (else (cons (car ls2)
                    (rem ls1 (cdr ls2))))))

;将列表集合ls1从列表集合ls2中删除
(define (rem* ls1 ls2)
  (cond ((null? ls1) ls2)
        (else (rem* (cdr ls1) (rem (car ls1) ls2)))))

;判断行row是否有可以存在皇后
(define (keizoku? saf row)
  (cond ((null? saf) #f)
        ((eq? (car (car saf)) row) #t)
        (else (keizoku? (cdr saf) row))))

(define (find-id id lst)
  (cond ((null? lst) "Not find!")
        ((eq? id (car lst))
         (car (cdr lst)))
        (else (find-id id (cdr lst)))))

(define (find-pos pos lst)
  (cond ((null? lst) "Not found!")
        ((same? pos (car lst))
         (car (cdr lst)))
        (else (find-pos pos (cdr lst)))))

(define (find lst row)
  (cond ((null? lst) '())
        ((eq? (car (car lst)) row)
         (cons (car lst)
               (find (cdr lst) row)))
        (else (find (cdr lst) row))))

(define lat '())
(define (memory+ act lst)
  (let ()
    (cond ((eq? act 'add)
           (set! lat (append lst lat)))
          ((eq? act 'cdr)
           (set! lat (cdr lst)))
          ((eq? act 'rem)
           (set! lat (rem lst lat)))
          ((eq? act 'zero)
           (set! lat '()))
          ((eq? act 'see) lat)
          (else "error"))
    lat))

;lst: (unsafe pos)
(define uns '())
(define (unsafe* pos lst)
  (let ()
    (set! uns (cons pos (cons lst uns)))
    uns))

(define (subst id lt lst)
  (cond ((null? lst) '())
        ((eq? id (car lst))
         (cons id
               (cons lt
                     (subst id lt (cdr (cdr lst))))))
        (else (cons (car lst)
                    (subst id lt (cdr lst))))))

;pst:皇后坐标序列(顺序为0->7)。返回坐标序列的unsafe集合
(define (comp-unsafe pst)
  (cond ((null? pst) '())
        (else (yui (unsafe (car pst))
                   (comp-unsafe (cdr pst))))))

;lst:(car mem)得到新坐标,并提取行坐标;way:坐标路径集合
(define (find-way lst way)
  (cond ((null? way) '())
        ((eq? (car lst) (car (car way))) (cdr way))
        (else (find-way lst (cdr way)))))

(define (memory saf mem)
  (cond ((null? saf) mem)
        ((eq? 'new saf) mem)
        (else (append (cdr saf) mem))))

(define (road saf way)
  (cond ((null? saf) way)
        (else (append (list (car saf)) way))))

(define (row mem)
  (cond ((null? (car mem)) 'end)
        (else (car (car mem)))))

(define (queens pos)
  (define (iter rid saf mem way ret)
    (cond ((and (null? mem)(null? saf)) ret)
          ((null? saf)
           (iter (+ 1 (car (car mem)))
                 'new
                 (cdr mem)
                 (append (list (car mem)) (find-way (car mem) way))
                 ret))
          ((= rid 8)
           (iter (+ 1 (car (car mem)))
                 'new
                 (cdr mem)
                 (append (list (car mem)) (find-way (car mem) way))
                 (append way (list /) ret)))
          (else (iter (+ rid 1)
                      (safe (comp-unsafe way) rid)
                      (memory saf mem)
                      (road (safe (comp-unsafe way) rid) way)
                      ret))))
  (iter 2 (safe (unsafe pos) 1) '() (road (safe (unsafe pos) 1) (list pos)) '()))

(define (all)
  (define (iter i j)
    (cond ((= j 8) '())
          (else (append (queens (list i j))
                        (iter i (+ j 1))))))
  (iter 0 0))