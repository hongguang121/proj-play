#lang racket
(require racket/gui/base)

;注意事项：所有文件内的括号全部使用全角
;读入字符串，然后转换为列表进行操作，最后再转换成字符串输出
;字符和字符之间只有一个有效制表符\t
;dsp可以按原格式显示字符串

(current-directory "f:/database/assembly")

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

;(open-file-string (list-ref (directory-list "f:/database/assembly") 5))


;opf
(define opf-1 (open-file-string (find-contents 1))) ;零部件总表
(define opf-2 (open-file-string (find-contents 2))) ;所有零部件总表
(define opf-3 (open-file-string (find-contents 3))) ;空气弹簧及传感器
(define opf-4 (open-file-string (find-contents 4))) ;库存项目合并表，确认发注点
(define opf-5 (open-file-string (find-contents 5))) ;库存总表(原始从未出库表)
(define zaiko (open-file-string (find-contents 6))) ;最新剩余库存总表（包含单价）

(define ay3015 (open-file-string (find-contents 8))) ;ay3015-160524
(define ay2415 (open-file-string (find-contents 9))) ;ay2415-160524
(define ay1511 (open-file-string (find-contents 10))) ;ay1511-160701
(define ay1209 (open-file-string (find-contents 11))) ;ay1209-1601
(define avt0405 (open-file-string (find-contents 12))) ;avt0405s-1601
(define adz1007 (open-file-string (find-contents 13))) ;adz1007-1601
(define adz0806 (open-file-string (find-contents 14))) ;adz0806-160701
(define ap200 (open-file-string (find-contents 15))) ;ap200-160101
(define dlj (open-file-string (find-contents 16))) ;独立脚-160101
(define opf-17 (open-file-string (find-contents 17))) ;兴源加工商加工零部件-160101
(define opf-18 (open-file-string (find-contents 18)))
(define gs (open-file-string (find-contents 19))) ;工数
(define ay4015 (open-file-string (find-contents 20)))

(define d-list
  `(("zaiko" ,zaiko)
    ("ay1209" ,ay1209)
    ("avt0405" ,avt0405)
    ("adz1007" ,adz1007)
    ("ap200" ,ap200)
    ("dlj" ,dlj)
    ("ay3015" ,ay3015)
    ("ay2415" ,ay2415)
    ("ay1511" ,ay1511)
    ("adz0806" ,adz0806)))


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

;找出列表中任意位置的值,ls是单列表
(define (find-list-id ls id)
  (cond ((= id 0) (car ls))
        (else (find-list-id (cdr ls) (- id 1)))))

;查询列表中是否存在查询的图号，如果存在则返回该列表结构的2,3,4位，ls是复合列表
(define (find-list-item ls it)
  (cond ((null? ls) '())
        ((string=? (caar ls) it)
         (cons (find-list-id (car ls) 2)
               (cons (find-list-id (car ls) 3)
                     (list (find-list-id (car ls) 4)))))
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

;去除(lst->str lst)中为最后一行多添加的"\r" str = (lst->str lst)
(define (no-r str)
  (substring str 0 (- (string-length str) 1)))

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


;str（图号）是否存在于表ls中 ls = ((extract opf) "图号")
(define (aru? ls str)
  (cond ((null? ls) #f)
        ((string=? (caar ls) str) #t)
        (else (aru? (cdr ls) str))))

;确认库存是否充足，若不足则提示 opf = (lsn-line opf ol)
;也可用于比较两张列表的不同项目（数量） opf - ol
(define (check opf ol)
  (define (iter lt th sl rem opf-ls)
    (cond ((null? lt) (reverse rem))
          ((or (not (aru? opf-ls th)) (< (same opf th) (string->number sl)))
           (iter (cdr lt) (caar lt) (cadar lt) (cons (list th sl) rem) opf-ls))
          (else (iter (cdr lt) (caar lt) (cadar lt) rem opf-ls))))
  (iter ((extract ol) "图号" "数量")
        (caar ((extract ol) "图号" "数量"))
        (cadar ((extract ol) "图号" "数量"))
        '()
        ((extract opf) "图号")))

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
               (iter (+ cnt (string-length (find-line opf lid col)) 1) 0 (+ rid 1) cid (string-append rem info "\t"))
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

;出库 存在bug
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

;找出全部含有str(图号)的行，并返回行号id ls = (ltt zaiko)
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

;(reverse (map (λ (x) (find-lsn (ltt zaiko) (car x))) (cdr (reverse ((extract ay1511)"图号""数量"))))) 暂时没用
;1.(define sr (cons (list "图号""数量""单价""位置")(reverse (cdr (reverse (seiri zaiko)))))) 定义整理后的库存表（去除零项）*
;(reverse (map (λ (x) (out (find-lsn sr (car x)) (cadr x) 1)) (cdr (reverse ((extract ay1511)"图号""数量"))))) 查找出库项目的位置*
;2.(dsp (lst->str (cons (list "图号" "剩余库存" "单价" "位置") (map (λ (x) (cdar x)) (reverse (map (λ (x) (out (find-lsn sr (car x)) (cadr x) 1)) (cdr (reverse ((extract ay1511)"图号""数量")))))))))

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

;出库多张表
(define ship-all
  (lambda args
    (let ([ls args])
      (letrec
          ((iter
            (lambda (lt cnt rem)
              (cond ((= (length ls) cnt) rem)
                    (else (iter (cdr lt) (+ cnt 1) (lst->str (allship rem (car lt)))))))))
        (iter ls 0 opf-5)))))  ;opf 为在库零部件表

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

;找出复合列表中的str行
(define (find-lss ls str)
  (cond ((null? ls) '())
        ((string=? (caar ls) str)
         (car ls))
        (else (find-lss (cdr ls) str))))

;出库 ol = ((extract opf) "") (find-list-id opf id)
;ls = (ltc opf)
(define (ship-lst opf ol)
  (reverse
   (change-line+ (find-ls (ltt opf) (caar ((extract ol) "图号" "数量")))
                 (out (find-last (find-ls (ltt opf) (caar ((extract ol) "图号" "数量")))) "8" (slt opf "数量")))))

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

;((extract opf-9) "图号""数量")
;合并a,b表中相同条目
(define (merge a b)
  (define (iter la lb lc ld rem)
    (cond ((string=? (caar la) "end") (append (list (list "图号" "名称" "数量" "单位")) ld lc (reverse rem)))
          ((string=? (caar lb) "end")
           (iter (cdr la) lc lc ld rem))
          ((string=? (caar la) (caar lb))
           (iter la
                 (cdr lb)
                 (delete lc (car la))
                 (delete ld (car la))
                 (cons
                  (cons (caar la)
                        (append (list (cadar la))
                                (list (number->string (+ (string->number (find-list-id (car la) 2))
                                                         (string->number (find-list-id (car lb) 2)))))
                                (list (find-list-id (car la) 3))))
                  rem)))
          (else (iter la (cdr lb) lc ld rem))))
  (iter ((extract a) "图号""名称""数量""单位")
        ((extract b) "图号""名称""数量""单位")
        ((extract b) "图号""名称""数量""单位")
        ((extract a) "图号""名称""数量""单位")
        '()))

;整理库存表，去掉其中数量为0的条目
(define (seiri opf)
  (define (iter ls rem)
    (cond ((null? ls) (reverse rem))
          ((string=? (find-list-id (car ls) 2) "0") (iter (cdr ls) rem))
          (else (iter (cdr ls) (cons (car ls) rem)))))
  (iter ((extract opf) "图号""名称""数量""单位""单价""时间""位置") '()))

;转换字符串为数字
(define (to-num str)
  (string->number str))

;转换数字为字符串
(define (to-str num)
  (number->string num))

;合并同一张表中的相同条目(包含数量，单价) lst = (seiri zaiko)
;并计算合计数量及平均价格
;id-num = 数量所在列序号（从零开始） id-price = 价格所在列序号
(define (make-set lst id-num id-price)
  (define (iter ls lt sum price rem remp)
    (cond ((string=? (caar ls) "end") (reverse rem))
          ((string=? (caar ls) "图号") (iter (cdr ls) remp sum price rem remp))
          ((null? lt)
           (iter (cdr ls)
                 remp
                 0
                 0
                 (cons
                  (cons (caar ls)
                        (cons (cadar ls)
                              (cons (to-str sum)
                                    (cons (to-str (/ price sum))
                                          (cons (to-str price)
                                                '())))))
                  rem)
                 remp))
          ((aru? rem (caar ls)) (iter (cdr ls) remp sum price rem remp))
          ((string=? (caar ls) (caar lt))
           (iter ls
                 (cdr lt)
                 (+ sum (to-num (find-list-id (car lt) id-num)))
                 (+ price (* (to-num (find-list-id (car lt) id-num)) (to-num (find-list-id (car lt) id-price))))
                 rem
                 remp))
          (else (iter ls (cdr lt) sum price rem remp))))
  (iter lst
        lst
        0
        0
        '()
        lst))

;(define la ((extract opf-4) "图号""名称"))
;(define lb (make-set (seiri opf-4)))
;(map (λ (x) (cons (mem? (car x) la) x)) lb) 为lb表添加图号

;合并多张表中的相同条目，一般用来计算出库零部件合计
(define merge-all
  (lambda args
    (let ([ls args])
      (letrec
          ((iter
            (lambda (lt cnt rem)
              (cond ((= cnt (- (length ls) 1)) rem)
                    (else (iter (cdr lt) (+ cnt 1) (no-r (lst->str (no-end (merge rem (car lt)))))))))))
        (iter (cdr ls) 0 (car ls))))))

;核算出库零部件总价
;自动根据单A的数据修改单B的数据

;(dsp (lst->str (allship (lst->str (allship (lst->str (allship (lst->str (allship (lst->str (allship (lst->str (allship opf-1 opf-8)) opf-9)) opf-11)) opf-12)) opf-13)) opf-16)))

;计算总额
;(apply + (map (lambda (x) (string->number x)) (cdr (reverse (map (lambda (x) (cadr x)) ((extract opf-6)"图号""单价"))))))

;通过图号寻找价格 price = 价格表 opf = 寻价表 ;必须保证库存充足的情况下进行核算，可用(check price opf)来确定是否库存充足
;价格列中不能出现空价格
(define (sear-price price opf)
  (define (iter pri lp spri num rem remp)  ;pri = ((extract price) "图号""数量""单价") lp = ((extract opf) "图号""数量") spri = 核算价格 num = 记录剩余商品数量
    (cond ((string=? (caar pri) "end")
           (iter remp lp 0 0 rem remp))
          ((string=? (caar lp) "end") (reverse rem))
          ((string=? (caar lp) "")
           (iter remp (cdr lp) 0 0 rem remp))
          ((string=? (caar lp) (caar pri))
           (if (> num (string->number (cadar pri)))
               (iter (cdr pri)
                     lp
                     (+ spri (* (string->number (cadar pri)) (string->number (caddar pri))))
                     (- num (string->number (cadar pri)))
                     rem
                     remp)
               (iter remp
                     (cdr lp)
                     0
                     (string->number (cadar (cdr lp)))
                     (cons (append (car lp)                                   
                                   (list (to-str (/ (+ spri (* num (to-num (caddar pri)))) (to-num (cadar lp)))))
                                   (list (to-str (+ spri (* num (to-num (caddar pri)))))))
                           rem)
                     remp)))
          (else (iter (cdr pri) lp spri num rem remp))))
  (iter ((extract price) "图号""数量""单价")
        ((extract opf) "图号""数量")
        0
        (string->number (cadar ((extract opf) "图号""数量")))
        '()
        ((extract price) "图号""数量""单价")))

;计算库存表每一条目的总价 (reverse (map (λ (x) (* (to-num (car x)) (to-num (cadr x)))) (cdr (reverse((extract zaiko)"数量""单价""图号")))))
;计算出库零部件总价（按照单个价格计算）(apply + (map (λ (x) (to-num (find-list-id x 2)))  (sear-price zaiko ay1511)))
;计算出库零部件总价（按照平均价格计算）(apply + (map (λ (x) (to-num (find-list-id x 2)))  (sear-price opf-4 ay1511)))

(define 显示 dsp) ;
(define 显示行号 dsp-lid) ;
(define 查询 sat) ;(查询 表单名 内容)
(define 出库 allship)
(define 检查库存 check)
(define 计算价格 sear-price)



;图形界面

(define frame (new frame%
                   [label "朝阳益同仓库管理程序"]
                   [width 960]
                   [height 640]
                   [style '(toolbar-button)]))

(define msg (new message% [parent frame]
                          [label "反馈结果......"])) 

(define panel (new horizontal-panel%
                   [parent frame]                   
                   [vert-margin 100]
                   [horiz-margin 50]
                   [min-width 50]
                   [min-height 15]))

(define pane (new horizontal-pane%
                  [parent frame]))

(define text (new text-field%
                  [label "显示结果"]
                  [parent frame]
                  [callback (lambda (t event)
                              '())]))

(define menu-bar (new menu-bar%
                      (parent frame)))

;(new menu%
;     (label "&File")
;     (parent menu-bar))

;(new menu%
;     (label "&Edit")
;     (parent menu-bar))

;(new menu%
;     (label "&Help")
;     (parent menu-bar))

;(new text-field%
;     [label "搜索栏"]
;     [parent panel])

(define gauge (new gauge%
                   (label "进度 ")
                   (parent panel)
                   (range 100)))
(define (sv cnt)
  (if (= cnt 100)
      100
      (sv (+ cnt 1))))

;(send gauge set-value (sv 0))

;(send frame show #t)

;功能说明：s = 字符串表; l = 列表; x = 字符串 id = 行号
;1.显示库存零部件及出库零部件 (dsp s) (dsp-id s)
;2.查询某个零部件信息并返回字符串表 (sat s x) ;可以和1组合 (search-all s x) ;包含表头
;3.通过行号查询任意一行内容并返回字符串表 (line-str s line-id)
;4.统计表中行数 (line-cnt s)
;5.查询纵列数据并返回列表 ((ext-line s id)"图号""名称"...) 查询某一行某几列数据 ;((extract s) "图号""名称"...) 查询整张表单某几列数据
;6.将列表转换成字符串并返回字符串表 (lst->str l)
;7.更新表单数据字符串 (update s id "名称" "EA7004") ;将表单为s，行号为id，列名为"名称"的信息更新为"EA7004"
;8.插入新数据 (insert s id (fmt "减压表 AW20-02-2-X2693 10 个"))
;9.删除表单数据 (del s id)
;10.将字符串表转换成列表 (ltt s) ;参见6
;11.出库 (allship s1 s2) s1 = 在库零部件表 s2 = 出库单 ;(ship-all s2 s3 ...) 从s2开始一次出库多张表
;12.计算出库零部件和并返回字符串表 (merge-all s1 s2 s3 ...) 从s1开始一次出库多张表
;13.列出表单价格 (sear-price s1 s2) s1 = 价格总表 s2 = 需要列出价格的子表

;添加并显示表头 (dsp (string-append "图号" "\t" "数量" "\t" "单价" "\r" (lst->str (sear-price opf-6 opf-18))))
;计算价格总和(出库零部件) (apply + (map (λ (x) (string->number x)) (map (λ (x) (caddr x)) (sear-price opf-6 opf-18))))
;计算价格总和(在库零部件) (apply + (map (λ (x) (if (or (string=? (car x) "end")(string=? (cadr x) "")) 0 (* (string->number (car x)) (string->number (cadr x))))) ((extract opf-6) "数量" "单价")))
;用append来连接多张表格 (append ((extract opf-8) "图号""数量") ((extract opf-9) "图号""数量"))
;将掉单价中的空价格改为"0" (map (λ (x) (if (string=? (cadr x) "") (cons (car x) (list "0")) x)) ((extract opf-6) "数量" "单价"))
;查找check后库存没有的条目 (map (λ (x) (search-all ay3015-1605 (car x))) (check zaiko ay3015-1605))
;1.检查库存 (check zaiko opf)
;2.出库 (dsp (lst->str (allship zaiko opf)))
;3.算价格 (dsp (lst->str (sear-price zaiko opf)))

;(define str (read))
;(symbol->string str)
;(map (lambda (x) ((extract (sat opf-1 (cadr x))) "图号""数量""单价")) (cdr (ltt opf-12))) ;查找出表opf-12中零部件在仓库中的所有库存和单价
;(ship-all ay1209-1601 ay3015-1605 ay2415-1605 avt0405-1601 adz1007-1601 ap200-1601 dlj-1601) ;出库
;(dsp (lst->str (sear-price opf-5 (merge-all ay1209-1601 avt0405-1601 adz1007-1601 ap200-1601 dlj-1601 ay3015-1605 ay2415-1605)))) 显示数张出库单的价格表
;(filter (λ (x) (if (string=? "end" (car x)) '() (and (not (= 0 (string->number (cadr x)))) (< (string->number (cadr x)) 10)))) ((extract zaiko) "图号" "数量")) 过滤非0库存中零部件低于10个的条目

;(define write-out (open-output-file "f:/database/kucun.txt"))
;(write (string->symbol zaiko) write-out)
;(close-output-port write-out)

;(printf "Please input a function:")
;(newline)
;(define sym (read-line))

;解析sym,通过空格将字符分开,返回列表
(define (bsk sym)
  (define (iter cnt word rem)
    (cond ((= (string-length sym) cnt) (reverse (cons word rem)))
          ((eq? #\space (string-ref sym cnt))
           (iter (+ cnt 1) "" (cons word rem)))
          (else (iter (+ cnt 1)
                      (string-append word (to-string (string-ref sym cnt)))
                      rem))))
  (iter 0 "" '()))

;确认字符是否是字符串列表d-list的成员 str = (cadr (bsk sym))
(define (mem? str d-list)
  (cond ((null? d-list) #f)
        ((string=? str (caar d-list)) (cadar d-list))
        (else (mem? str (cdr d-list)))))

;通过选测器来执行通过bsk分析后的函数 x = (bsk sym)
(define (select x)
  (cond ((null? x) '())
        ((string=? (car x) "dsp")
         (if (mem? (cadr x) d-list)
             (display-to-file (string->symbol (mem? (cadr x) d-list)) "f:/test2.txt" #:mode 'text #:exists 'update)             
             "Paramenter error"))
        ((string=? (car x) "check")
         (if (mem? (cadr x) d-list)
             (check zaiko (mem? (cadr x) d-list))
             "参数错误"))
        ((string=? (car x) "ship")
         (if (mem? (cadr x) d-list)
             (dsp (lst->str (allship zaiko (mem? (cadr x) d-list))))
             "参数错误"))
        ((string=? (car x) "search")         
         (dsp (sat zaiko (cadr x))))
        (else "Paramenter error")))

;(define (start x)
;  (if (string=? (car (bsk sym)) "quit")
;      "See you!"
;      x))

;(start (select (bsk sym)))


;(define lx ((extract zaiko) "图号"))
;(define ly ((extract gs) "图号""工数"))
;(map (λ (x) (if (mem? (car x) ly) (cons (car x) (list (mem? (car x) ly))) (cons (car x) (list "no")))) lx) 合并工数

;(define lx ((extract zaiko) "图号""名称"))
;(define ly ((extract opf-4) "图号""数量""单价""发注点"))
;(map (λ (x) (cons (car x) (cons (mem? (car x) lx) (cdr x)))) ly) 将lx表每一条图号后添加名称后返回

;发注点检查 发注点列表 opf-4
;在每次出库后，需要用剩余库存和发注点列表莱重新计算有无需要订购的零部件
;(filter (λ (x) (not (null? x))) (map (λ (x) (if (<= (to-num (cadr x)) (to-num (caddr x))) x '())) (cdr (reverse ((extract opf-4)"图号""数量""发注点""名称")))))
;发注点表
;(define la (cdr (reverse ((extract opf-4)"图号""数量""发注点""名称")))) 
;(filter (λ (x) (not (null? x)))
;        (map (λ (x) (if (<= (to-num (find-list-id x 1)) (to-num (find-list-id (assoc (car x) la) 2))) x '()))
;               (make-set (seiri (no-r (lst->str (allship zaiko ay1511)))))))

;按图号排序opf-4表中的数据
;(define lx ((extract opf-4) "图号""名称""数量""单价""发注点"))
;(dsp (lst->str (map (λ (x) (search-all opf-4 x)) (sort (map (λ (x) (car x)) ((extract opf-4)"图号")) string<?))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; new add ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;直接显示在文件中
;(display-to-file (string->symbol ay2415) "f:/test2.txt" #:mode 'text #:exists 'update)
;以文本模式写入到文件中,如果文件存在则更新
;(write-to-file (string->symbol ay2415) "f:/test2.txt" #:mode 'text #:exists 'update)

;(dsp(no-r(lst->str(map (λ (x) (if (null? (find-list-item part-1 (car x))) (cons (car x) (cons (cadr x) (cons "" (cons "" (cons "" (cons (cddr x) '())))))) (find-lss part-1 (car x)))) ((extract ay1511) "图号""名称""数量""单价""金额")))))

;删除复合列表数据
(define (rem ls str)
  (cond ((null? ls) '())
        ((string=? (caar ls) str) (cdr ls))
        (else (cons (car ls)
                    (rem (cdr ls) str)))))

;格式化删除p2已有数据后的p1
(define (format-p p1)
  (cond ((null? p1) '())
        (else (cons
               (append (car p1)
                       '(""""""))
               (format-p (cdr p1))))))

;入库 p1 = part-1 p2 = 入库单，包含"图号""名称""数量""单价""金额"
(define (store p1 p2)
  (define (iter p1 p2 remb)
    (cond ((null? p2) (append (format-p p1) (reverse remb)))
          ((null? (find-list-item p1 (caar p2)))
           (iter p1
                 (cdr p2)
                 (cons
                  (cons (caar p2)
                        (cons (cadar p2)
                              (cons ""
                                    (cons ""
                                          (cons ""
                                                (cddar p2))))))
                  remb)))
          (else (iter (rem p1 (caar p2))
                      (cdr p2)                      
                      (cons (append (find-lss p1 (caar p2))
                                    (find-list-item p2 (caar p2)))
                            remb)))))
  (iter p1 p2 '()))


(define (store+ p1 p2)
  (define (iter p1 p2 remb)
    (cond ((null? p1) (append (format-p p2) (reverse remb)))
          ((null? (find-list-item p2 (caar p1)))
           (iter (cdr p1)
                 p2
                 (cons
                  (append (car p1) '(""""""))
                  remb)))
          (else (iter p1
                      (rem p2 (caar p1))                      
                      (cons (append p1
                                    (find-list-item p2 (caar p1)))
                            remb)))))
  (iter p1 p2 '()))

;1.当前在库状况
;(define part-1 (cons '("图号""名称""数量""单价""金额")(make-set (seiri zaiko) 2 4)))
;格式化part-1
;(dsp (no-r (lst->str part-1)))
;2.零部件入库 opf = 入库单 均去掉表头
;(define part-2 (cons '("图号""名称""数量""单价""金额""数量""单价""金额") (store (cdr part-1) (cdr (ltt opf-18)))))
;格式化part-2
;(dsp (lst->str part-2))
;合并库存表和入库单
;(define new-part (append (append part-1 (cdr (ltt opf-18))) '(("end""end""end""end""end"))))
;同入库合并后的新库存表，用于出库
;(define part-3 (cons '("图号""名称""数量""单价""金额")(make-set new-part 2 3)))
;格式化part-2
;(dsp (no-r (lst->str (cons '("图号""名称""数量""单价""金额""数量""单价""金额") (store (cdr part-2) (cdr (ltt opf)))))))
;3.零部件出库
;出库单价格表 返回"图号""数量""单价""金额"
;(define part-4 (sear-price (lst->str part-3) ay1511))
;格式化出库价格表part-4,增添名称列
;(define part-4s (map (λ (x y) (append (list (find-list-id y 0)) (list (car x)) (cdr y))) (cdr (ltt ay1511)) part-4))
;将出库价格表part-4s添加到part-2中
;(define part-5 (map (λ (x) (if (null? (find-list-item part-4s (car x))) (append x '("""""")) (append x (find-list-item part-4s (car x))))) (cdr part-2)))
;格式化part-5
;(dsp (no-r (lst->str (cons '("图号""名称""数量""单价""金额""数量""单价""金额""数量""单价""金额") part-5))))
;4.出库后的新库存表part-3 - part-4
;(define part-6
;  (map (λ (x)
;         (if (null? (find-list-item part-4s (car x)))
;             (append (find-lss part-5 (car x)) (find-list-item part-3 (car x)))
;             (append (find-lss part-5 (car x))
;                     (list (to-str (- (to-num (find-list-id x 2)) (to-num (find-list-id (find-lss part-5 (car x)) 8)))))
;                     (list (if (= (- (to-num (find-list-id x 2)) (to-num (find-list-id (find-lss part-5 (car x)) 8))) 0)
;                               "0"
;                               (to-str (/ (- (to-num (find-list-id x 4)) (to-num (find-list-id (find-lss part-5 (car x)) 10)))
;                                          (- (to-num (find-list-id x 2)) (to-num (find-list-id (find-lss part-5 (car x)) 8)))))))
;                     (list (to-str (- (to-num (find-list-id x 4)) (to-num (find-list-id (find-lss part-5 (car x)) 10))))))))
;       (cdr part-3)))
;格式化part-6
;(dsp (no-r (lst->str (cons '("图号""名称""数量""单价""金额""数量""单价""金额""数量""单价""金额""数量""单价""金额") part-6))))


;月末盘点表 出库单 = (merge-all ...) (盘点表 zaiko opf-18 (merge-all ay1511 adz0806)) 如果表单为空，就用""代替
;需要添加入库和出库时间的比较，从而按顺序出入库，而非先入再出
(define (盘点表 库存表 入库单 出库单)
  (begin
    (define part-1 (cons '("图号""名称""数量""单价""金额")(make-set (seiri 库存表) 2 4)))
    (define part-2 (cons '("图号""名称""数量""单价""金额""数量""单价""金额") (store (cdr part-1) (cdr (ltt 入库单)))))
    (define new-part (append (append part-1 (cdr (ltt 入库单))) '(("end""end""end""end""end"))))
    (define part-3 (cons '("图号""名称""数量""单价""金额")(make-set new-part 2 3)))
    (define part-4 (sear-price (lst->str part-3) 出库单))
    (define part-4s (map (λ (x y) (append (list (find-list-id y 0)) (list (car x)) (cdr y))) (cdr (ltt 出库单)) part-4))
    (define part-5 (map (λ (x) (if (null? (find-list-item part-4s (car x))) (append x '("""""")) (append x (find-list-item part-4s (car x))))) (cdr part-2)))
    (define part-6
      (map (λ (x)
             (if (null? (find-list-item part-4s (car x)))
                 (append (find-lss part-5 (car x)) (find-list-item part-3 (car x)))
                 (append (find-lss part-5 (car x))
                         (list (to-str (- (to-num (find-list-id x 2)) (to-num (find-list-id (find-lss part-5 (car x)) 8)))))
                         (list (if (= (- (to-num (find-list-id x 2)) (to-num (find-list-id (find-lss part-5 (car x)) 8))) 0)
                                   "0"
                                   (to-str (/ (- (to-num (find-list-id x 4)) (to-num (find-list-id (find-lss part-5 (car x)) 10)))
                                              (- (to-num (find-list-id x 2)) (to-num (find-list-id (find-lss part-5 (car x)) 8)))))))
                         (list (to-str (- (to-num (find-list-id x 4)) (to-num (find-list-id (find-lss part-5 (car x)) 10))))))))
           (cdr part-3)))
    ;(write-to-file (string->symbol (no-r (lst->str (cons '("图号""名称""数量""单价""金额""数量""单价""金额""数量""单价""金额""数量""单价""金额") part-6))))
    ;               "f:/test2.txt" #:mode 'text #:exists 'update)
    (dsp (no-r (lst->str (cons '("图号""名称""数量""单价""金额""数量""单价""金额""数量""单价""金额""数量""单价""金额") part-6))))
    ))

;计算ay4015的总价
;(apply + (map to-num (map (λ (x) (find-list-id x 3)) (sear-price zaiko ay4015))))