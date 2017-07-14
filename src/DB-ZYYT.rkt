#lang racket
(require racket/gui/base)

;注意事项：所有文件内的括号全部使用全角
;读入字符串，然后转换为列表进行操作，最后再转换成字符串输出
;字符和字符之间只有一个有效制表符\t
;dsp可以按原格式显示字符串


(define fopen "f:/database/contents.txt")

(define contents (file->list fopen))

;fc = find-contents
(define (find-contents id)
  (define (iter ctent)
    (cond ((null? ctent) "Can't find file.")
          ((= (car ctent) id) (symbol->string (car (cdr ctent))))
          (else (iter (cddr ctent)))))
  (iter contents))

;ops = open-file-string
(define (open-file-string fname)
  (file->string fname))

;(open-file-list (find-contents id))
(define (open-file-list fname)
  (file->list fname))

;opf
(define opf-1 (open-file-string (find-contents 1))) ;零部件总表
(define opf-2 (open-file-string (find-contents 2))) ;所有零部件总表
(define opf-3 (open-file-string (find-contents 3))) ;空气弹簧及传感器
(define opf-4 (open-file-string (find-contents 4))) ;截止16.5.31仓库库存
(define opf-5 (open-file-string (find-contents 5))) ;库存总表
(define opf-6 (open-file-string (find-contents 6))) ;日元价格

(define opf-8 (open-file-string (find-contents 8))) ;ay3015-160524
(define opf-9 (open-file-string (find-contents 9))) ;ay2415-160524
(define opf-10 (open-file-string (find-contents 10))) ;ay1511-
(define opf-11 (open-file-string (find-contents 11))) ;ay1209-1601
(define opf-12 (open-file-string (find-contents 12))) ;avt0405s-1601
(define opf-13 (open-file-string (find-contents 13))) ;adz1007-1601
(define opf-14 (open-file-string (find-contents 14))) ;adz0806
(define opf-15 (open-file-string (find-contents 15))) ;ap200-1601
(define opf-16 (open-file-string (find-contents 16))) ;独立脚-1601
(define opf-17 (open-file-string (find-contents 17))) ;兴源加工商加工零部件-1601
(define opf-18 (open-file-string (find-contents 18))) 


;(dsp (lst->str (map (lambda (x) (sat opf-11 (car x))) (check opf-1 opf-11)))) ;显示opf-11中opf-1没有项目的名称

;过滤掉字符串中另起新行的"\n"和空格
(define (flt opff)
  (define (iter cnt rem)
    (cond ((= (string-length opff) cnt) rem)
          ((string=? (to-string (string-ref opff cnt)) "\n")
           (iter (+ cnt 1) rem))
          ((eq? (string-ref opff cnt) #\space)
           (iter (+ cnt 1) rem))
          (else (iter (+ cnt 1) (string-append rem (to-string (string-ref opff cnt)))))))
  (iter 0 ""))

;用symbol格式化字符串数据
(define (dsp opf)
  (string->symbol (flt opf)))

;带行号
(define (dsp-lid opf)
  (string->symbol (line-id (flt opf))))

(define (find-list-id ls id)
  (cond ((= id 0) (car ls))
        (else (find-list-id (cdr ls) (- id 1)))))

(define (find-list-item ls it)
  (cond ((null? ls) error)
        ((eq? (car ls) it) (car ls))
        (else (find-list-item (cdr ls) it))))

;(directory-list )
;(path-string? )
;(file-exists? )
;(write-to-file str path)

;#\x => "x"
(define (to-string str-ref)
  (list->string (list str-ref)))

;查询一个字符是否是一串字符的子集
(define (exist-string? single complex)
  (define (iter start end check)
    (cond ((> (string-length single) (string-length complex)) #f)
          ((= (string-length complex) 0) #f)
          ((= start end)
           (cond ((= (string-length complex) end) #f)
                 (else (iter 0 (+ end 1) (substring complex 0 (+ end 1))))))
          ((string=? single check) #t)
          (else (iter (+ start 1) end (substring complex (+ start 1) end)))))
  (iter -1 1 ""))

;只保留一个制表符
(define (table opf cnt)
  (cond ((string=? (to-string (string-ref opf (+ cnt 1))) "\t")
         (table opf (+ cnt 1)))
        (else (+ cnt 1))))

;显示文本内容
;opff = open-file-string
;(display-file-string (open-file-string (find-contents id)))
(define (display-file-string opff)
  (define (iter cnt line rem)
    (cond ((= (string-length opff) cnt) (string->symbol line))
          ((string=? (to-string (string-ref opff cnt)) "\t")
           ;(iter (+ cnt 1) line (string-append rem "\t")))       ;保留文件原样
           (iter (table opff cnt) line (string-append rem "\t"))) ;保留一个制表符
          ((string=? (to-string (string-ref opff cnt)) "\r")
           (iter (+ cnt 1) (string-append rem "\r") rem))
          (else (iter (+ cnt 1) line (string-append rem (to-string (string-ref opff cnt)))))))
  (iter 1 "" ""))

;导出任意一行内容,从第0行计数
;(dsp (line-str (open-file-string (find-contents id)) lid))
(define (line-str opff lid)
  (define (iter lcnt cnt rem)
    (cond ((= cnt (string-length opff)) "end")                    ;如果超出行号则显示结束
          ((and (string=? (to-string (string-ref opff cnt)) "\r")(= lcnt lid)) rem)
          ((string=? (to-string (string-ref opff cnt)) "\n")
           (iter lcnt (+ cnt 1) rem))
          ((string=? (to-string (string-ref opff cnt)) "\r")
           (iter (+ lcnt 1) (+ cnt 1) ""))
          ((string=? (to-string (string-ref opff cnt)) "\t")
           (iter lcnt (+ cnt 1) (string-append rem "\t")))        ;保留一个制表符
          (else (iter lcnt (+ cnt 1) (string-append rem (to-string (string-ref opff cnt)))))))
  (iter 0 0 ""))

;为opff添加行号
;(dsp (line-id opff))
(define (line-id opff)
  (define (iter lid rem)
    (cond ((string=? (line-str opff lid) "end") (string-append rem "\r"))
          (else (iter (+ lid 1)
                      (string-append rem "\r" (number->string lid) "\t" (line-str opff lid))))))
  (iter 1 (string-append " " "序号" "\t" (line-str opff 0))))

;总行数，从1起算
(define (line-cnt opf)
  (define (iter lid)
    (cond ((string=? (line-str opf lid) "end") lid)
          (else (iter (+ lid 1)))))
  (iter 0))

;可以通过一行中的任何一个字符串检索出整行内容,不区分大小写
;(dsp (search-line (open-file-string (find-contents 14)) "59b"))
(define (search-line opff str)
  (define (iter cnt check rem)
    (cond ((= (string-length opff) cnt) '());(format "Not find ~a." str))
          ((exist-string? (string-upcase str) (string-upcase check))
           (cond ((string=? (to-string (string-ref opff cnt)) "\r") (string-append (line-str opff 0) "\r" rem))
                 (else (iter (+ cnt 1) check (string-append rem (to-string (string-ref opff cnt)))))))
          ((string=? (to-string (string-ref opff cnt)) "\n")
           (iter (+ cnt 1) "" rem))
          ((string=? (to-string (string-ref opff cnt)) "\r")
           (iter (+ cnt 1) check ""))
          ((string=? (to-string (string-ref opff cnt)) "\t")
           (iter (+ cnt 1) "" (string-append rem "\t")))
          (else (iter (+ cnt 1)
                      (string-append check (to-string (string-ref opff cnt)))
                      (string-append rem (to-string (string-ref opff cnt)))))))
  (iter 1 "" "")) 

;查找所有具有相同名称的项目
;(dsp (search-all opff "59b"))
(define (search-all opff str)
  (define (iter cnt check rem)
    (cond ((= (string-length opff) cnt) "")
          ((exist-string? (string-upcase str) (string-upcase check))
           (cond ((string=? (to-string (string-ref opff cnt)) "\r")
                  (string-append rem "\r" (iter (+ cnt 1) "" "")))
                 (else (iter (+ cnt 1) check (string-append rem (to-string (string-ref opff cnt)))))))
          ((string=? (to-string (string-ref opff cnt)) "\n")
           (iter (+ cnt 1) "" rem))
          ((string=? (to-string (string-ref opff cnt)) "\r")
           (iter (+ cnt 1) check ""))
          ((string=? (to-string (string-ref opff cnt)) "\t")
           (iter (+ cnt 1) "" (string-append rem "\t")))          
          (else (iter (+ cnt 1)
                      (string-append check (to-string (string-ref opff cnt)))
                      (string-append rem (to-string (string-ref opff cnt)))))))
  (iter 1 "" ""))

;检索结果加入标题行 sat = search-all+title
;(dsp (sat opff "pc"))
(define (sat opff str)
  (string-append (line-str opff 0) "\r" (search-all opff str)))

;通过列名查找纵列数据
;找出列名序号
;title = (line-str opff 0)
(define (search-title opf str)
  (define (iter title cnt cntb check)
    (cond ((string=? str check) cntb)
          ((= (string-length title) cnt) 0)          
          ((string=? (to-string (string-ref title cnt)) "\t")
           (iter title (table title cnt) (+ cntb 1) ""))
          (else (iter title (+ cnt 1) cntb (string-append check (to-string (string-ref title cnt)))))))
  (iter (line-str opf 0) 1 0 ""))

;计算标题数目
(define (title-cnt opf)
  (define (iter title cnt cntb)
    (cond ((= (string-length title) cnt) cntb)
          ((string=? (to-string (string-ref title cnt)) "\t")
           (iter title (table title cnt) (+ cntb 1)))
          (else (iter title (+ cnt 1) cntb))))
  (iter (line-str opf 0) 1 0))

;找出纵列数据 str = 纵列标题
;(dsp (search-col opff str))
(define (search-col opff str)
  (define (iter tid line lid cnt cntb rem)
    (cond ((= cnt (string-length line))
           (iter tid (line-str opff lid) (+ lid 1) 0 0 (string-append rem "\r")))
          ((string=? (to-string (string-ref line cnt)) "\t")
           (iter tid line lid (+ cnt 1) (+ cntb 1) rem))
          ((= tid cntb)
           (cond ((or (string=? (to-string (string-ref line cnt)) "\t")
                      (string=? (to-string (string-ref line cnt)) "\n"))
                  (iter tid line lid 0 1 rem))
                 (else (iter tid line lid (+ cnt 1) cntb (string-append rem (to-string (string-ref line cnt)))))))
          ((string=? line "end") rem)
          (else (iter tid line lid (+ cnt 1) cntb rem))))
  (iter (search-title opff str) (line-str opff 0) 1 0 0 ""))

;查找一行中的纵列数据
;line = (line-str opf id)单行字符串 str = 标题
(define (find-line opf lid str)
  (define (iter line tid cnt cntb check)
    (cond ((= cnt (string-length line)) check)
          ((= tid cntb)
           (if (string=? (to-string (string-ref line cnt)) "\t")
               check
               (iter line tid (+ cnt 1) cntb (string-append check (to-string (string-ref line cnt))))))
          ((string=? (to-string (string-ref line cnt)) "\t")
           (iter line tid (+ cnt 1) (+ cntb 1) ""))          
          (else (iter line tid (+ cnt 1) cntb (string-append check (to-string (string-ref line cnt)))))))
  (iter (line-str opf lid) (search-title opf str) 0 0 ""))

;加入纵列标题行
;(dsp (sct opff str))
(define (sct opff str)
  (string-append str "\r" (search-col opff str)))

;做格式整理，将读入信息按照一个制表符分隔，不含空格，如果某列没有值，则补空
;(dsp (form (open-file-string (find-contents 16))))
(define (form opff)
  (define (iter cnt rem)
    (cond ((= (string-length opff) cnt) rem)
          ((eq? (string-ref opff cnt) #\space)
           (iter (+ cnt 1) rem))      ;去掉不必要的空格
          ((string=? (to-string (string-ref opff cnt)) "\r")
           (if (string=? (to-string (string-ref rem (- (string-length rem) 1))) "\t")
               (iter (+ cnt 1) (string-append (substring rem 0 (- (string-length rem) 1)) "\r"))
               (iter (+ cnt 1) (string-append rem "\r"))))
          ((string=? (to-string (string-ref opff cnt)) "\t")
           (iter (table opff cnt) (string-append rem "\t")))
          (else (iter (+ cnt 1) (string-append rem (to-string (string-ref opff cnt)))))))
  (iter 0 ""))

;string->number
;list-string
;sum

;出库 sk = 仓库 out = 出库单
;opf = 出库部品表 args = 图号，数量等信息；做成字符串列表
;((extract opf-19 2) "图号" "数量" "名称") 抽取列元素组成新表
;未来需要提升效率和增加字符串级别抽取
;找出一行
(define ext-line
  (lambda (opf lid)
    (lambda args
      (let loop ([ls args])
        (if (null? ls)
            '()    
            (cons (find-line opf lid (car ls))
                  (loop (cdr ls))))))))

;找出整张表
(define extract
  (lambda (opf)
    (lambda args
      (let ([ls args]
            [lt (line-cnt opf)])
        (letrec
            ((iter
              (lambda (lid lss rem)
                (cond ((> lid lt) '())
                      ((null? lss)
                       (cons (reverse rem)
                             (iter (+ lid 1) ls '())))
                      (else
                       (iter lid (cdr lss) (cons (find-line opf lid (car lss)) rem)))))))
          (iter 1 ls '()))))))

;判定是不是一个列表的最后一个元素
(define (last? ls)
  (if (= (length ls) 1)
      #t
      #f))

;将列表转换成字符串显示
(define (lst->str lst)
  (cond ((null? lst) "\r")        
        ((not (pair? (car lst)))
         (if (last? lst)
             (string-append (car lst) (lst->str (cdr lst)))
             (string-append (car lst) "\t" (lst->str (cdr lst)))))
        (else (string-append (lst->str (car lst))
                             (lst->str (cdr lst))))))

;从A表中减去B表中相应项目的数目
;合并A表中相同名称或者
;计算出表opf中项目str的总数 如：(same opf-1 "2693")
;目前入库品中未加入 黏着剂 接头 气管 护角 螺丝 插板

(define (same opf str)
  (define (iter ls lid lcnt sum)
    (cond ((= lcnt lid) sum)
          (else (iter ls lid (+ lcnt 1) (+ sum (string->number (find-line ls lcnt "数量")))))))
  (iter (sat opf str) (line-cnt (sat opf str)) 1 0))

;列出opf表中ol的部分，若没有则用“没有图号”代替 ol = ((extract opf) "图号")
;(dsp (lst->str (map (lambda (x) (search-all opf-9 (car x))) (check opf-1 opf-9)))) 检查opf-1表中opf-9没有的部分
(define (lsn-line opf ol)
  (define (iter lt rem nfd)
    (cond ((string=? (caar lt) "end") (string-append rem nfd))
          ((null? (search-line opf (caar lt)))
           (iter (cdr lt) rem (string-append nfd "Not_find_" (caar lt) "\r\n")))
          (else (iter (cdr lt) (string-append rem (search-all opf (caar lt))) nfd))))
  (iter ((extract ol) "图号") "" ""))

;确认库存是否充足，若不足则提示 opf = (lsn-line opf ol)
(define (check opf ol)
  (define (iter lt th sl rem)
    (cond ((null? lt) (reverse rem))
          ((and (not (string=? th "")) (< (same opf th) (string->number sl)))
           (iter (cdr lt) (caar lt) (cadar lt) (cons (list th sl) rem)))
          (else (iter (cdr lt) (caar lt) (cadar lt) rem))))
  (iter ((extract ol) "图号" "数量") "" "" '()))

;修改读入字符串中的数据，包括更新，添加(string-append)，删除
;通过序号lid更新位于col列的数据 opf = (line-id opf)
;(dsp-lid (update opf-16 22 "名称" "EA7004"))
(define (update opf lid col info)
  (define (iter cnt cntb rid cid rem)
    (cond ((= (string-length opf) cnt) rem)          
          ((string=? (to-string (string-ref opf cnt)) "\r")
           (iter (+ cnt 1) 0 (+ rid 1) cid (string-append rem "\r")))
          ((string=? (to-string (string-ref opf cnt)) "\t")
           (iter (+ cnt 1) (+ cntb 1) rid cid (string-append rem "\t")))
          ((= lid rid)
           (if (= cid cntb)
               (iter (+ cnt (string-length (find-line opf lid col)) 1) 0 (+ rid 1) cid (string-append rem info))
               (iter (+ cnt 1) cntb rid cid (string-append rem (to-string (string-ref opf cnt))))))
          (else (iter (+ cnt 1)
                      cntb
                      rid
                      cid                      
                      (string-append rem (to-string (string-ref opf cnt)))))))
  (iter 0 0 0 (search-title opf col) ""))

;整理插入字符串,字符串中间用空格隔开
;(fmt "名称 图号 数量 价格")
(define (fmt str)
  (define (iter cnt rem)
    (cond ((= (- (string-length str) 1) cnt)
           (string-append (string-append rem (to-string (string-ref str cnt))) "\r\n"))          
          ((eq? (string-ref str cnt) #\space)
           (iter (+ cnt 1) (string-append rem "\t")))
          (else (iter (+ cnt 1) (string-append rem (to-string (string-ref str cnt)))))))
  (iter 0 ""))

;从任意位置（行号）插入一行新数据strln = "传感器安装板\t19B0783D-5\t2\t个\r" 现在strln = (fmt "str")
(define (insert opf id strln)
  (define (iter cnt lid rem)
    (cond ((= (string-length opf) cnt) rem)
          ((string=? (to-string (string-ref opf cnt)) "\r")
           (iter (+ cnt 1) (+ lid 1) (string-append rem "\r")))
          ((= lid id)
           (iter cnt (+ lid 1) (string-append rem strln)))
          (else (iter (+ cnt 1) lid (string-append rem (to-string (string-ref opf cnt)))))))
  (iter 0 0 ""))

;删除任意一行（通过行号）
(define (del opf id)
  (define (iter cnt lid rem)
    (cond ((= (string-length opf) cnt) rem)
          ((string=? (to-string (string-ref opf cnt)) "\r")
           (iter (+ cnt 1) (+ lid 1) (string-append rem "\r")))
          ((= lid id)
           (iter (+ cnt (string-length (line-str opf id)) 2) (+ lid 1) rem))
          (else (iter (+ cnt 1) lid (string-append rem (to-string (string-ref opf cnt)))))))
  (iter 0 0 ""))

;计算两个字符型数字差值，若大于零则返回结果，若小于零则返回零，并记录剩余数字
;用sn1-sn2
(define (sub sn1 sn2)
  (cond ((>= (- (string->number sn1) (string->number sn2)) 0)
         (number->string (- (string->number sn1) (string->number sn2))))
        (else (cons "0" (number->string (- (string->number sn2) (string->number sn1)))))))

;处理sn1-sn2小于零的情形 res = (sub sn1 sn2)
(define (ctn res)
    (if (pair? res)
        (car res)
        res))

(define (ctn? res)
    (if (pair? res)
        #t
        #f))

;出库
;opf = 库存表 ol = 出库单中的一行（图号和数量） ol = (car ((extract opf) "图号" "数量"))
(define (ship opf ol)
  (define (iter cnt cntb lid rid check rem res th sl cid)
    (cond ((= (string-length opf) cnt) rem)          
          ((string=? (to-string (string-ref opf cnt)) "\r")
           (iter (+ cnt 1) 0 (+ lid 1) rid "" (string-append rem "\r") res th sl cid))
          ((string=? (to-string (string-ref opf cnt)) "\t")
           (iter (+ cnt 1) (+ cntb 1) lid rid "" (string-append rem "\t") res th sl cid))
          ((eq? #t res)
           (iter cnt cntb lid rid check rem #f th (cdr (sub (find-line opf lid "数量") sl)) cid))
          ((or (string=? check th) (= lid rid))
           (if (= cid cntb)
               (iter (+ cnt (string-length (find-line opf lid "数量")))
                     0 lid 0 ""
                     (string-append rem (ctn (sub (find-line opf lid "数量") sl)))
                     (ctn? (sub (find-line opf lid "数量") sl))
                     th sl cid)
               (iter (+ cnt 1) cntb lid lid check (string-append rem (to-string (string-ref opf cnt))) res th sl cid)))
          (else (iter (+ cnt 1)
                      cntb
                      lid
                      rid
                      (string-append check (to-string (string-ref opf cnt)))
                      (string-append rem (to-string (string-ref opf cnt)))
                      res
                      th
                      sl
                      cid))))
  (iter 0 0 0 0 "" "" #f (car ol) (cadr ol) (search-title opf "数量")))

;格式整理，找出一列中的最长字符长度，对其余不足这个长度的字符增补空格
;将字符串文件转换为列表
;最后一行后面必须连接换行符"\n"
(define (to-list opf)
  (define (iter cnt check)
    (cond ((= cnt (string-length opf)) '())
          ((or (string=? (to-string (string-ref opf cnt)) "\r")
               (string=? (to-string (string-ref opf cnt)) "\t"))
           (cons check
                 (iter (+ cnt 1) "")))
          ((string=? (to-string (string-ref opf cnt)) "\n")
           (iter (+ cnt 1) check))
          (else (iter (+ cnt 1)
                      (string-append check (to-string (string-ref opf cnt)))))))
  (iter 0 ""))

;用列表读入文件，每一行为一个子列表 ltt = list-title-count
(define (ltt opf)
  (define (iter ls tc cnt rem res)
    (cond ((null? ls) (append (reverse res) (list (reverse rem))))
          ((= tc cnt)
           (iter ls tc 0 '() (cons (reverse rem) res)))
          (else (iter (cdr ls) tc (+ cnt 1) (cons (car ls) rem) res))))
  (iter (to-list opf) (+ 1 (title-cnt opf)) 0 '() '()))

;找到行号id起内容 ls = (ltt opf)
(define (lst ls id)  
    (cond ((null? ls) '())
          ((= id 0) (cons (car ls) (lst (cdr ls) id)))
          (else (lst (cdr ls) (- id 1)))))

;找出全部含有str(图号)的行，并返回行号id ls = (ltt opf)
(define (find-lsn ls str)
  (define (iter lt id res)
    (cond ((null? lt)
           (if (null? res)
               ls
               (reverse res)))
          ((string=? (caar lt) str)
           (iter (cdr lt) (+ id 1) (cons (cons (car lt) (list id))
                                         res)))
          (else (iter (cdr lt) (+ id 1) res))))
  (iter ls 0 '()))

;slt = search-list-title tr = title-str
(define (slt opf tr)
  (define (iter ls cnt)
    (cond ((null? ls) 0)
          ((string=? (car ls) tr) cnt)
          (else (iter (cdr ls) (+ cnt 1)))))
  (iter (car (ltt opf)) 0))

;将传入列表找到对应列并进行运算 ls = (ltt opf) sl = 出库数量 tc = 库存列表“数量”列号
;fl = (find-lsn ls str)
;(define out-id '()) ;记录出库行id
;(out (find-lsn (ltt opf-1) "59B1217Z") "36" 2)
(define (out fl sl tc)
  (define (iter fll ls id cnt sl bol rem) ;ls:单行数据 bol:判定是否继续出库
    (cond ((null? ls)           
           (if (eq? bol #t)
               (cons (cons id (reverse rem))
                     (iter (cdr fll) (caar fll) (cadar fll) 0 sl #f '()))
               (cons (cons id (reverse rem)) '())))
          ((and (eq? bol #t) (null? fll))
           "库存不足！")
          ((= tc cnt)
           ;(set! out-id (cons id out-id))
           (if (ctn? (sub (car ls) sl))                                  
               (iter fll (cdr ls) id (+ cnt 1) (cdr (sub (car ls) sl)) #t (cons (ctn (sub (car ls) sl)) rem))
               (iter fll (cdr ls) id (+ cnt 1) sl bol (cons (ctn (sub (car ls) sl)) rem))))
    (else (iter fll (cdr ls) id (+ cnt 1) sl bol (cons (car ls) rem)))))
  (iter (cdr fl) (caar fl) (cadar fl) 0 sl #f '()))

;通过行号替换列表中存在的行 ls = (ltt opf) ln = (out ls sl tc)
(define (change-line ls ln)
  (define (iter ls ln id line res)
    (cond ((null? ls)
           (if (null? ln)
               (reverse res)
               (iter (reverse res) (cdr ln) (caar ln) (cdar ln) '())))
          ((= id 0)
           (iter (cdr ls) ln (- id 1) line (cons line res)))
          (else (iter (cdr ls) ln (- id 1) line (cons (car ls) res)))))
  (iter ls (cdr ln) (caar ln) (cdar ln) '()))

;opf = 库存表 ol = 出库单中的一行（图号和数量） oll = ((extract opf) "图号" "数量")
(define (allship opf ol)
  (define (iter-1 ls lt tc)
    (define (iter-2 lt fl th sl res)
      (cond ((null? lt) res)
            (else (iter-2 (cdr lt)
                          (find-lsn res (caar lt))
                          (caar lt)
                          (cadar lt)                          
                          (change-line res (out fl sl tc))))))      
    (iter-2 (cdr lt)
            (find-lsn ls (caar lt))
            (caar lt)
            (cadar lt)            
            (change-line ls (out (find-lsn ls (caar lt)) (cadar lt) tc))))
  (iter-1 (ltt opf)
          ((extract ol) "图号" "数量")
          (slt opf "数量")))

(define ship-all
  (lambda args
    (let ([ls args])
      (letrec
          ((iter
            (lambda (lt cnt rem)
              (cond ((= (- (length ls) 1) cnt) rem)
                    (else (iter (cdr lt) (+ cnt 1) (lst->str (allship rem (car lt)))))))))
        (iter ls 0 opf-1)))))

;找出最后一行数据
(define (find-last ls)
  (car (reverse ls)))

;将修改后的数据插入表中，替换原有数据行
(define (change-line+ ls ln)
  (cons ln (cdr (reverse ls))))

;查找复合表中数据,通过(find-last ls)来去除查找结果
(define (find-ls ls str)
  (define (iter ls rem)
    (cond ((null? ls) '())
          ((string=? (caar ls) str)
           (append (reverse rem) (list (car ls))))
          (else (iter (cdr ls) (cons (car ls) rem)))))
  (iter ls '()))

;出库 ol = ((extract opf) "") (find-list-id opf id)
;ls = (ltc opf)
(define (ship-lst opf ol)
  (reverse
   (change-line+ (find-ls (ltt opf) (caar ((extract ol) "图号" "数量")))
                 (out (find-last (find-ls (ltt opf) (caar ((extract ol) "图号" "数量")))) "8" (slt opf "数量")))))

;图形界面

(define frame (new frame%
                   [label "朝阳益同仓库管理"]
                   [width 960]
                   [height 640]
                   [style '(toolbar-button)]))

(define msg (new message% [parent frame]
                          [label "反馈结果......"])) 

(define my-canvas%
  (class canvas% ; The base class is canvas%
    ; Define overriding method to handle mouse events
    (define/override (on-event event)
      (send msg set-label "Canvas mouse"))
    ; Define overriding method to handle keyboard events
    (define/override (on-char event)
      (send msg set-label "Canvas keyboard"))
    ; Call the superclass init, passing on all init args
    (super-new)))
 
; Make a canvas that handles events in the frame
(new my-canvas% [parent frame])

(new canvas% [parent frame]
             [paint-callback
              (lambda (canvas dc)
                (send dc set-scale 3 3)
                (send dc set-text-foreground "blue")
                (send dc draw-text "Don't Panic!" 0 0))])

(new button% [parent frame]
             [label "Click Me"]
             ; Callback procedure for a button click:
             [callback (lambda (button event)
                         (send msg set-label "Button click"))])

(define panel (new horizontal-panel% [parent frame]))

(new button% [parent panel]
             [label "Left"]
             [callback (lambda (button event)
                         (send msg set-label "Left click"))])

(new button% [parent panel]
             [label "Right"]
             [callback (lambda (button event)
                         (send msg set-label "Right click"))])

(define menu-bar (new menu-bar%
                      (parent frame)))

(new menu%
     (label "&File")
     (parent menu-bar))

(new menu%
     (label "&Edit")
     (parent menu-bar))

(new menu%
     (label "&Help")
     (parent menu-bar))

(new text-field%
     [label "搜索栏"]
     [parent panel])

(send frame show #t)

;(define str (read))
;(symbol->string str)
;(map (lambda (x) ((extract (sat opf-1 (cadr x))) "图号""数量""单价")) (cdr (ltt opf-12))) ;查找出表opf-12中零部件在仓库中的所有库存和单价
(define 显示 dsp) ;
(define 显示行号 dsp-lid) ;
(define 查询 sat) ;(查询 表单名 内容)

;判断连个list是否相等

;将a从ls中删除
(define (delete ls a)
  (cond ((null? ls) '())
        ((string=? (caar ls) (car a))
         (delete (cdr ls) a))
        (else (cons (car ls) (delete (cdr ls) a)))))

;去除列表中的"end"行
(define (no-end ls)
  (cond ((null? ls) '())
        ((string=? (caar ls) "end")
         (no-end (cdr ls)))
        (else (cons (car ls)
                    (no-end (cdr ls))))))

;合并a，b表中相同项
;((extract opf-9) "图号""数量")
(define (merge a b)
  (define (iter la lb lc ld rem)
    (cond ((string=? (caar la) "end") (append (list (list "图号" "数量" "名称" "单位")) ld lc (reverse rem)))
          ((string=? (caar lb) "end")
           (iter (cdr la) lc lc ld rem))
          ((string=? (caar la) (caar lb))
           (iter la
                 (cdr lb)
                 (delete lc (car la))
                 (delete ld (car la))
                 (cons
                  (cons (caar la)
                        (append (list (number->string (+ (string->number (cadar la))
                                                 (string->number (cadar lb)))))
                                (cddar la)))
                  rem)))
          (else (iter la (cdr lb) lc ld rem))))
  (iter ((extract a) "图号""数量""名称""单位")
        ((extract b) "图号""数量""名称""单位")
        ((extract b) "图号""数量""名称""单位")
        ((extract a) "图号""数量""名称""单位")
        '()))

(define merge-all
  (lambda args
    (let ([ls args])
      (letrec
          ((iter
            (lambda (lt cnt rem)
              (cond ((= cnt (- (length ls) 1)) rem)
                    (else (iter (cdr lt) (+ cnt 1) (lst->str (no-end (merge rem (car lt))))))))))
        (iter (cdr ls) 0 (car ls))))))

;核算出库零部件总价
;自动根据单A的数据修改单B的数据

;(dsp (lst->str (allship (lst->str (allship (lst->str (allship (lst->str (allship (lst->str (allship (lst->str (allship opf-1 opf-8)) opf-9)) opf-11)) opf-12)) opf-13)) opf-16)))

;计算总额
;(apply + (map (lambda (x) (string->number x)) (cdr (reverse (map (lambda (x) (cadr x)) ((extract opf-6)"图号""单价"))))))

;通过图号寻找价格 price = 价格表 opf = 寻价表 ;必须保证库存充足的情况下进行核算，可用(check price opf)来确定是否库存充足
(define (sear-price price opf)
  (define (iter pri lp spri num rem remp)  ;pri = ((extract price) "图号""数量""单价") lp = ((extract opf) "图号""数量") spri = 核算价格 num = 记录剩余商品数量
    (cond ((string=? (caar pri) "end")
           (iter remp lp 0 0 rem remp))
          ((string=? (caar lp) "end") (reverse rem))
          ((string=? (caar lp) (caar pri))
           (if (> num (string->number (cadar pri)))
               (iter (cdr pri)
                     lp
                     (* (string->number (cadar pri)) (string->number (caddar pri)))
                     (- num (string->number (cadar pri)))
                     rem
                     remp)
               (iter remp
                     (cdr lp)
                     0
                     (string->number (cadar (cdr lp)))
                     (cons (append (car lp)
                                   (list (number->string (+ spri (* num (string->number (caddar pri)))))))
                           rem)
                     remp)))
          (else (iter (cdr pri) lp spri num rem remp))))
  (iter ((extract price) "图号""数量""单价")
        ((extract opf) "图号""数量")
        0
        (string->number (cadar ((extract opf) "图号""数量")))
        '()
        ((extract price) "图号""数量""单价")))

;添加并显示表头 (dsp (string-append "图号" "\t" "数量" "\t" "单价" "\r" (lst->str (sear-price opf-6 opf-18))))
;计算价格总和 (apply + (map (λ (x) (string->number x)) (map (λ (x) (caddr x)) (sear-price opf-6 opf-18))))

