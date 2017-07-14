#lang racket

(require db)
(require racket/serialize
         racket/future)
;(require avl)
(require html)
(require xml
         xml/path)
(require racket/fasl)
;(require 2htdp/batch-io
;         2htdp/image
;         2htdp/universe)

;不同语言对接可以采用数据对接，将数据导出成公用格式即可(将输出结果导入文件)。测试代码是否有问题即通过输入数据然后看输出是否合理。总之，数据是根本，切记！
;因为服务器扮演容器的角色，而各种语言均可连接服务器并从中进行存取操作，所以多语言对接就可以通过服务器来作为中介完成。
;因为超乎常理，所以才耐人寻味。
;外语学习，最重要的是注意到各词语间发音的细微区别以及语义的细微区别，而后自己输出，即说或者写，通过使用来学习，而不是死记硬背。
;次序对性能的影响很大，需要注意。目前自己编的程序相较于库程序而言性能很差，在需求性能的情况下，需要慎重。另外应该学习库函数高效率的原因。
;不要被别人的想法带着走，自己的思考是至关重要的。
;为了加快搜索速度，是否可以将单词的字母连接转换为数字索引，然后通过数字搜索？
;通过future和touch进行并行计算来提升性能
;也可以用promise进行lazy computation (force (delay/thread "Laziness")) 因为结果会被缓存
;测试的本质是证明，即证明某个函数是否成立。
;应该向音乐学习编程技巧和本质

;英文单词备忘
;可做多语言查询

;查询主要通过(find-en str)(find-cn str)(findm str)
;添加单词用(add '(str str))
;添加词组用(adds '(str str))


;制作全面的单词原型表，以便于生词查找时忽略变形，也可以不做此表，而是通过将所有形式都输入其中来做查询，当然前者更好一些，也更困难。

;(define dir (directory-list "E:/Source/words/"))
;需要保护文件的安全性，防止被误修改等操作
(define wlist (future (λ () (stream-first (stream->list (stream (file->string "e:/source/words/wlist.txt")))))))        ;我的单词表
;(define f (s-exp->fasl mywords)) ;直接将list mywords转换为字节码f
;(write-to-file f "e:/source/words/wlist" #:mode 'binary) ；将字节码f直接写入文件
;(define wlist (future (λ () (fasl->s-exp (file->value "e:/source/words/wlist"))))) ;将字节码文件读入并转换为list
(define slist (future (λ () (stream-first (stream->list (stream (file->string "e:/source/words/slist.txt")))))))        ;我的词组表
;(define slist (future (λ () (fasl->s-exp (file->value "e:/source/words/slist")))))
;(define vocab (delay (file->list "e:/source/words/vocabulary.txt")))     ;6000词频表
;(define v15000 (delay (file->string "e:/source/words/v15000.txt")))      ;15000词频表
;(define gre (file->string "e:/source/words/gre.txt"))            ;GRE词汇
;(define simple (file->list "e:/source/words/simple.txt"))        ;简单词表
(define idlist (future (λ () (file->string "e:/source/words/identical.txt"))))   ;特殊变形表
;(define idlist (future (λ () (fasl->s-exp (file->value "e:/source/words/id")))))
(define org (file->list "e:/source/words/origin.txt"))           ;动词原形表
(define dict+ (delay (file->lines "e:/source/words/dict.txt")))          ;牛津英汉
(define han (delay (file->string "e:/source/words/han.txt")))            ;汉字
;(define han-word (file->string "e:/source/words/han-word.txt"))  ;汉语词组
(define article (future (λ () (file->string "e:/source/words/test.txt"))))       ;文本
(define nov2666 (delay (file->string "e:/source/words/2666.txt")))
(define novgwtw (delay (file->string "e:/source/words/gone_with_the_wind.txt")))
(define moby_dick (delay (file->string "e:/source/words/melville-moby_dick.txt")))

(define (pre-load)
  (let ()
    (set! wlist (touch wlist))
    (set! slist (touch slist))
    (set! idlist (touch idlist))
    (set! article (touch article))))

(pre-load)
  
(define (words/to-string str-ref)
  (list->string (list str-ref)))

;计时器
(define mytime current-inexact-milliseconds)

;(super-apply sqr * 2 + 1 1)
(define (super-apply . args)  
  (let loop ((ls (reverse args))
             (res '()))
    (cond ((null? ls) (car res))
          ((procedure? (car ls))
           ;(loop (cdr ls) (append (list apply (car ls)) (list res))))
           (loop (cdr ls) (list (apply (car ls) res))))
          (else (loop (cdr ls) (cons (car ls) res))))))

;example: (how-long search* nov2666 "Morini literature")
;也可以使用系统计时器(time (fn))，但会返回函数值
(define (how-long fn . para)
  (let ((start (mytime))
        (res (apply fn para)))
    (- (mytime) start)))

;格式整理单词表
(define (format* ls sym)
  (define (select)
    (cond ((eq? 'wlist sym) (iter ls 0 2 '() '()))          
          ((eq? 'vocab sym) (iter ls 0 4 '() '()))
          ((eq? 'slist sym) (fstr ls 0 0 "" '() '()))
          ((eq? 'idlist sym) (idstr ls 0 "" '() '()))))
  (define (iter lt cnt len rem res)
    (cond ((null? lt) (append res (list rem)))
          ((= cnt len) (iter lt 0 len '() (append res (list rem))))
          (else (iter (cdr lt) (+ cnt 1) len (append rem (list (car lt))) res))))
  (define (fstr str cnt line check rem res)
    (cond ((= line 2)
           (fstr str cnt 0 "" '() (cons rem res)))
          ((= (string-length str) cnt) (reverse res))
          ((string=? (words/to-string (string-ref str cnt)) "\r")
           (fstr str (+ cnt 1) line "" (append rem (list check)) res))
          ((string=? (words/to-string (string-ref str cnt)) "\n")
           (fstr str (+ cnt 1) (+ line 1) "" rem res))          
          (else (fstr str (+ cnt 1) line (string-append check (words/to-string (string-ref str cnt))) rem res))))
  (define (idstr str cnt check rem res)
    (cond ((= (string-length str) cnt) (reverse res))
          ((string=? (words/to-string (string-ref str cnt)) "\n")
           (idstr str (+ cnt 1) "" '() (cons (reverse rem) res)))
          ((or (string=? (words/to-string (string-ref str cnt)) "\r")
               (string=? (words/to-string (string-ref str cnt)) "\t")
               (string=? (words/to-string (string-ref str cnt)) " "))
           (idstr str (+ cnt 1) "" (cons check rem) res))
          (else (idstr str (+ cnt 1) (string-append check (words/to-string (string-ref str cnt))) rem res))))
  (select))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define mywords (format* wlist 'slist)) ;旧读入方式，采用字符串文本通过格式转换得到
;(define mywords (touch wlist)) ;新读入方式，采用Fast-Load Serialization
(define mysets (format* slist 'slist))
;(define mysets (touch slist))
(define id (format* idlist 'idlist))
;(define id (touch idlist))
;(define gw (format* gre 'slist))
;(define freqw (format* vocab 'vocab))
;(set! v15000 (map reverse (format* v15000 'slist)))

;排序mywords
(define (sort-mywords)
  (let ((title (car mywords))
        (void '()))
    (set! mywords (sort (cdr mywords) string<=? #:key car))
    (set! mywords (cons title mywords))
    (delete-file "e:/source/words/wlist.txt")
    (set! void (map (λ (x) (reuse x "e:/source/words/wlist.txt" 'append)) mywords))))

;定义avl-tree
;(define tree (make-avl (λ (x y) (string<=? (car x) (car y)))))
;(define proc (for/list ((v (in-list mywords))) (avl-add! tree v)));将mywords加入树中
;(for/list ((v (in-avl tree))) v);遍历树

;过滤出词频表freqw中的单词
;(define fws (map symbol->string (map (λ (x) (list-ref x 2)) freqw)))

;***已改用add函数添加新词
;向单词库中增添陌生单词 (addw '(cascade "瀑布")) ;释义现在改用string来表达更复杂的形式
;为了防止格式错误，应该添加格式验证函数
;(define (addw word)
;  (if (not (assq (car word) mywords))
;      (let ((mean (cdr word)))
;        (if (or (null? mean)
;                (regexp-match #rx"," (car mean)))
;            "格式错误！"
;            (begin 
;              (display-lines-to-file word "e:/source/words/wlist.txt" #:mode 'text #:exists 'append)
;              (set! wlist (file->string "e:/source/words/wlist.txt"))
;              (set! mywords (map (λ (x) (cons (string->symbol (car x)) (cdr x))) (format* wlist 'slist))))))
;      (assq (car word) mywords)))

;***以字符串的形式添加单词 sentence = '("wipe out" "毁灭，失败，失衡")
(define (reuse ls path exists)
  (display-lines-to-file ls path #:mode 'text #:exists exists)) ;写入单条列表，如果是整个列表则需要(map (λ (x) (reuse x "e:/source/words/wlist.txt" 'append)) mywords)
  ;(write-to-file ls path #:mode 'binary #:exists exists))
  
(define (add word)
  (if (and (not (assf (λ (x) (string=? (car word) x)) mywords))(= (length word) 2))
      (begin
        (reuse word "e:/source/words/wlist.txt" 'append)
        ;(reuse (s-exp->fasl word) "e:/source/words/wlist" 'append)
        (set! wlist (file->string "e:/source/words/wlist.txt"))
        (set! mywords (format* wlist 'slist)))
        ;(set! mywords (fasl->s-exp (file->value "e:/source/words/wlist"))))
      (assf (λ (x) (string=? (car word) x)) mywords)))

;添加短语，短句 (adds '("wipe out" "毁灭，失败，失衡"))
(define (adds sentence)
  (if (not (assf (λ (x) (string=? (car sentence) x)) mysets))
      (begin
        (reuse sentence "e:/source/words/slist.txt" 'append)        
        (set! slist (file->string "e:/source/words/slist.txt"))
        (set! mysets (format* slist 'slist)))
      (assf (λ (x) (string=? (car sentence) x)) mysets)))

;修改某词条 new = '("cascade" "瀑布") old = mywords 注意：间隔逗号一定要用中文逗号
;如果想快速更新某个条目，应该用索引的方式将该条目提取出来(list-ref)(substring)，然后更改该条后再插回原索引处
(define (change new)
  (let ()
    (del (car new))
    (add new)))

;删除单词表中的词条（硬盘） word = 'del
(define (del word)
  (if (assf (λ (x) (string=? word x)) mywords)
      (begin 
        (set! mywords (remove* (list word) mywords (λ (x y) (string=? x (car y)))))
        (delete-file "e:/source/words/wlist.txt")
        (map (λ (x) (reuse x "e:/source/words/wlist.txt" 'append)) mywords) ;重写文件，费时
        (set! wlist (file->string "e:/source/words/wlist.txt"))
        (set! mywords (format* wlist 'slist)))
      "Not Find."))

;转换单词表为字符串格式 know = 已经记住的单词表
;(define swrd (map symbol->string (map car words)))
;用英文查询单词表 (assq x words)
;str为单词字符串，支持模糊查询
(define words-cache '()) ;保存当前查询过的单词

(define (find-en str)
  (if (string=? str "")
      "请输入要查询的单词"
      (let* ((res (filter
                   (λ (x)
                     (or (string-contains? (car x) (string-downcase str))
                         (string-contains? (string-downcase str) (car x))))
                   mywords))
             (str (string-trim (string-downcase str)))
             (word (filter
                    (λ (x)
                      (string=? (car x) str))
                    res)))
        (if (not (null? word))
            (if (not (findf (λ (x) (string=? (car x) str)) words-cache))
                (begin
                  (set! words-cache (cons (car word) words-cache))
                  res)
                res)
            res))))

;用中文查找单词表
(define (find-cn str)
  (filter (λ (x) (string-contains? (cadr x) str)) mywords))

;单词测试 name = 单词表 (test mywords) 按n键继续,其他键退出
(define (test name)
  (if (null? name) "全部测试完毕"
      (let* ((ref (random (length name)))
             (word (car (list-ref name ref)))
             (mean (cadr (list-ref name ref))))
        (display word)
        (display " ————> ")
        (let ((rd (read)))
          (cond ((eq? 'n rd)
                 (begin
                   (display mean)
                   (set! name (remove word name))
                   (newline)
                   (test name)))
                (else "测试结束"))))))

;过滤文章中多余的符号 args = 需过滤的符号集，string类型
;注意sign标志，意思是将连续的几个符号替换成1个空格，有几个符号就是几
;也可以用(string-trim)函数，但它无法去掉句子中间的符号
;example: (string-filter article "\r" "\n")
(define (string-filter str . args)
  (define (iter sign cnt rem)
    (cond ((= (string-length str) cnt) rem)
          ((and (< (+ cnt 1) (string-length str))  ;跳过"."后面的空格和","(根据情况)
                (string=? (words/to-string (string-ref str cnt)) ".")
                (or (string=? (words/to-string (string-ref str (+ cnt 1))) " ")
                    (string=? (words/to-string (string-ref str (+ cnt 1))) ",")))
           (iter sign (+ cnt 2) (string-append rem (words/to-string (string-ref str cnt)))))          
          ((and (>= cnt 1)
                (eq? #f (not (member (words/to-string (string-ref str cnt)) args)))
                (not (string=? (words/to-string (string-ref str (- cnt 1))) "."))  ;如果需要过滤的符号前是"."，则不在它之后添加空格                
                (not (string=? (words/to-string (string-ref rem (- (string-length rem) 1))) " "))  ;如果已经有了空格则不再添加
                (= sign 4))
           (iter 0 (+ cnt 1) (string-append rem " ")))
          ((eq? #f (not (member (words/to-string (string-ref str cnt)) args)))                
           (iter 0 (+ cnt 1) rem))
          (else (iter 4 (+ cnt 1) (string-append rem (words/to-string (string-ref str cnt)))))))
  (iter 4 0 ""))

;切分文章可以用(string-split)函数

;从文件中抽离单词(apply mkset `(,article))，可用来单词计数wordcount = (length (draw (string-filter nov2666 "\r" "\n")))
(define (draw str)
  (define (iter cnt check rem)
    (cond ((= (string-length str) cnt) (reverse (cons check rem)))          
          ((regexp-match #rx"[\n|\t|,|.|“|”|(|)|:|;|?|—|%|$|‘|’]"
                         (words/to-string (string-ref str cnt)))
           ;(or (string=? (to-string (string-ref str cnt)) "\n")
            ;   (string=? (to-string (string-ref str cnt)) "\t")               
            ;   (string=? (to-string (string-ref str cnt)) ",")
            ;   (string=? (to-string (string-ref str cnt)) ".")
            ;   (string=? (to-string (string-ref str cnt)) "\"")
            ;   ;(string=? (to-string (string-ref str cnt)) "\'")
            ;   (string=? (to-string (string-ref str cnt)) "“")
            ;   (string=? (to-string (string-ref str cnt)) "”")
            ;   (string=? (to-string (string-ref str cnt)) "(")
            ;   (string=? (to-string (string-ref str cnt)) ")")
            ;   (string=? (to-string (string-ref str cnt)) ":")
            ;   (string=? (to-string (string-ref str cnt)) ";")
            ;   (string=? (to-string (string-ref str cnt)) "?")
            ;   (string=? (to-string (string-ref str cnt)) "—")
            ;   ;(string=? (to-string (string-ref str cnt)) "-")
            ;   (string=? (to-string (string-ref str cnt)) "%")
            ;   (string=? (to-string (string-ref str cnt)) "$")
            ;   (string=? (to-string (string-ref str cnt)) "‘")
            ;   (string=? (to-string (string-ref str cnt)) "’"))
           (iter (+ cnt 1) check rem))
          ((or (eq? (string-ref str cnt) #\space)
               (string=? (words/to-string (string-ref str cnt)) "\r"))
           (iter (+ cnt 1) "" (cons check rem)))
          (else (iter (+ cnt 1) (string-append check (words/to-string (string-ref str cnt))) rem))))
  (iter 0 "" '()))

;过滤掉抽离的重复单词 在nov2666的author's上有bug
(define (mkset str)
  (define (iter lt check cnt rem)
    (cond ((null? lt) (reverse rem))
          ((string=? (car lt) check) (iter (cdr lt) check (+ cnt 1) rem))
          (else (iter (cdr lt) (car lt) 1 (cons (cons check cnt) rem)))))
  (iter (sort (map string-downcase (draw str)) string<?) "" 1 '())) ;排序后进行单词计数会提升效率，因为相同的单词被放在了一起

;全部小写化 (map string-downcase list)
;过滤出词频表中没有出现的词汇 (filter (λ (x) (not (member x fws))) (mkset article))
;(map (λ (x) (filter (λ (y) (string=? x y)) fws)) (mkset article))

;截取列表中的一段
(define (cut ls start end)
  (define (iter ls cnt rem)
    (cond ((null? ls) (reverse rem))
          ((and (>= cnt start)
                (< cnt end))
           (iter (cdr ls) (+ cnt 1) (cons (car ls) rem)))
          ((= end cnt) (reverse rem))
          (else (iter (cdr ls) (+ cnt 1) rem))))
  (iter ls 0 '()))

;判断列表成员
(define (member? a ls)
  (cond ((null? ls) #f)
        ((member a (car ls)) (car ls))
        (else (member? a (cdr ls)))))

;终极的解决方式是将所有的单词（除去规则无歧义变形外）都加入单词表
;整理相近词，辨识出同一单词的不同变形，不过滤词性 w1 / w2 : string        ;需要区分特有名词的大写和一般名字句子开头的大写
;特殊变形需要从变形表中确认 变形表 = id
(define (id? w1 w2)
  (let* ((w1 (car (draw w1)))
         (w2 (car (draw w2)))
         (len1 (string-length w1))
         (len2 (string-length w2)))   
    (define (iter w1 w2 len1 len2)
      (cond ((member? w1 id)
             (and (member? w2 id)
                  (string=? (car (member? w1 id)) (car (member? w2 id)))))   ;born bear drove drive
            ((and (= (- len1 len2) 1)(> len2 2))
             (and (or (and (regexp-match #rx"s$" w1)
                           (regexp-match #rx"[^s]$" w2))                     ;more mores
                      (and (regexp-match #rx"d$" w1)
                           (regexp-match #rx"e$" w2)))
                  (string-contains? w1 w2)))
            ((and (>= (- len1 len2) 2)(> len2 2)(< (- len1 len2) 5))
             (or (and (regexp-match #rx"s$" w1)
                      (string=? w2 (substring w1 0 (- len1 1))))
                 (and (regexp-match #rx"'s$|'m$" w1)
                      (string=? w2 (substring w1 0 (- len1 2))))
                 (and (regexp-match #rx"ing$" w1)
                      (regexp-match #rx"e$" w2)
                      (string-contains? w2 (substring w1 0 (- len1 3))))
                 (and (regexp-match #rx"ing$" w1)
                      (regexp-match #rx"[d|l|n|t|y]$" w2)                      
                      (string=? w2 (to-origin w1)));(substring w1 0 (- len1 3)))) ;writ writing
                 (and (regexp-match #rx"ning$" w1)
                      (regexp-match #rx"n$" w2)                      
                      (string=? w2 (substring w1 0 (- len1 4))))
                 (and (regexp-match #rx"ly$" w1)
                      (regexp-match #rx"[e|d|g|i|r|s|t|l]$" w2)
                      (string=? w2 (substring w1 0 (- len1 2))))
                 (and (regexp-match #rx"ed$" w1)
                      (string=? w2 (substring w1 0 (- len1 2))))
                 (and (regexp-match #rx"ies$|ied$" w1)
                      (regexp-match #rx"y$" w2)
                      (string-contains? w2 (substring w1 0 (- len1 3))))
                 (and (regexp-match #rx"ity" w1)
                      (string=? w2 (substring w1 0 (- len1 3))))
                 (and (regexp-match #rx"er$|es$" w1)
                      (regexp-match #rx"[g|l]$" w2)
                      (string-contains? w2 (substring w1 0 (- len1 2))))
                 (and (regexp-match #rx"ten$|ted$" w1)
                      (regexp-match #rx"e$" w2)
                      (string-contains? w2 (substring w1 0 (- len1 2))))
                 (and (regexp-match #rx"ring$" w1)                        ;ear earring
                      (string=? w2 (substring w1 0 (- len1 4))))
                 (and (regexp-match #rx"ful$|est$" w1)
                      (regexp-match #rx"g$" w2)
                      (char=? (string-ref w1 0)(string-ref w2 0))
                      (string=? w2 (substring w1 0 (- len1 3))))
                 ;(and (regexp-match #rx"ion$" w1)                ;not notion 因为ion多数情况是让动词名词化，所以此条目应该加入单词表而不应该用来判断单词是否相同
                 ;     (regexp-match #rx"[e|t]$" w2)              ;动词的可能形式able也可以考虑是否加入
                 ;     (string=? w2 (substring w1 0 (- len1 3))))
                 (and (regexp-match #rx"ions$" w1)
                      (regexp-match #rx"[e|t]$" w2)
                      (string-contains? w2 (substring w1 0 (- len1 4))))
                 ))
            (else (string=? w1 w2))))
    (if (string=? w1 w2)
        #t
        (if (>= len1 len2)
            (iter w1 w2 len1 len2)
            (iter w2 w1 len2 len1)))))

;将变形后的单词恢复原形 同时需要一个原型表用来比对 word:string *未完成* 
(define (to-origin word)
  (define (iter len)
    (cond ((not (null? (filter (λ (x) (member word x)) id))) (car (member? word id))) ;从动词变形表中查找
          ((not (null? (filter (λ (x) (eq? (string->symbol word) x)) org))) word) ;从动词原型表中查找
          ((or (char=? (string-ref word (- len 1)) #\d)
               (char=? (string-ref word (- len 1)) #\s))
           (substring word 0 (- len 1)))
          ((string=? (substring word (- len 2) len) "ed")
           (substring word 0 (- len 2)))
          ((string=? (substring word (- len 3) len) "ing")
           (string-append (substring word 0 (- len 3)) "e"))))
  (iter (string-length word)))

;过滤掉mkset列表中的相近词 
;(define (id-filter art)

;过滤出我的生词表中没有的单词，简单词和变形除外
(define (unknown art)
  (let ((lst (filter (λ (x) (not (string->number x)))
              (filter (λ (x) (> (string-length x) 3))
                      (map car (mkset article))))))
    (filter (λ (x) (eq? #f (member x mywords ;(λ (x y) (id? x (car y))) ;不通过id?函数判定
                                   ))) lst)))
    
    
;过滤出单词"gun": (filter (λ (x) (string=? x "gun")) (map string-downcase (draw article)))
;过滤出长度大于2的单词： (filter (λ (x) (> (string-length x) 2)) (mkset article))
;过滤掉数字: (filter (λ (x) (not (string->number x))) (mkset article))

;搜索文章，不能隔词搜索 example:(search (string-filter nov2666 "\r" "\n") "Morini" 25)，但可以分断搜索,目前问题是性能很差
;编写正则表达式版本
(define (search art str len)
  (thread (λ ()
  (let ((art+ (string-downcase art))
        (str+ (string-downcase str)))
    (define (iter alen slen cnt rem)
      (if (string-contains? art+ str+)
          (cond ((= (+ cnt slen) alen) (printf rem));将查询结果的前后25个字母显示出来
                ((string=? (substring art+ cnt (+ cnt slen)) str+)
                 (cond ((and (> cnt len) (< (+ cnt slen len) alen))
                        (iter alen slen (+ cnt 1) (string-append rem (substring art (- cnt len) (+ cnt slen len)) "\r")))
                       ((< (+ cnt slen len) alen);解决单词在文首的情况
                        (iter alen slen (+ cnt 1) (string-append rem (substring art cnt (+ cnt slen len)) "\r")))
                       (else;(> cnt len);解决单词在文末的情况
                        (iter alen slen (+ cnt 1) (string-append rem (substring art (- cnt len) alen) "\r")))))
                (else (iter alen slen (+ cnt 1) rem)))
          (error "Didn't find" str)))
    (iter (string-length art) (string-length str) 0 "")))))

(define (findw art str)  
  (let* ((L1 (string-length art))
         (L2 (string-length str))
         (d (- L1 L2)))    
    (or (zero? L2)
        (let loop ((start 0))                   
          (and (<= start d)
               (or (let loop2 ((offset 0))
                     (or (= offset L2)
                         (and (char=? (char-downcase (string-ref art (+ start offset)))
                                      (string-ref str offset))
                              (loop2 (add1 offset)))))
                   (loop (add1 start))))))))

;通过索引查找dict+中单词和释义
(define (index ch)
  (case (char->integer ch)
    ((65 97) (cons 0 4749))((66 98) (cons 4749 10008))((67 99) (cons 10008 18523))((68 100) (cons 18523 23847))
    ((69 101) (cons 23847 26934))((70 102) (cons 26934 31185))((71 103) (cons 31185 34261))((72 104) (cons 34261 37731))
    ((73 105) (cons 37731 41735))((74 106) (cons 41735 42525))((75 107) (cons 42525 43189))((76 108) (cons 43189 46363))
    ((77 109) (cons 46363 51031))((78 110) (cons 51031 52669))((79 111) (cons 52669 54829))((80 112) (cons 54829 62570))
    ((81 113) (cons 62570 63032))((82 114) (cons 63032 67674))((83 115) (cons 67674 78602))((84 116) (cons 78602 83390))
    ((85 117) (cons 83390 85116))((86 118) (cons 85116 86507))((87 119) (cons 86507 89193))((88 120) (cons 89193 89227))
    ((89 121) (cons 89227 89537))((90 122) (cons 89537 89675))))
;***使用member速度很快
(define (findm word)
  (let* ((ch (string-ref word 0))
         (start (car (index ch)))
         (end (cdr (index ch))))
    (let loop ((id start))               
      (cond ((= id end) '("not found" "not found"))
            ((regexp-match (regexp (string-append "^" word)) (list-ref (force dict+) id))
             (cons word (list (list-ref (force dict+) (+ id 1)))))
            (else (loop (add1 id)))))))
;  (let ((res (member word dict+ (λ (x y) (regexp-match (regexp (string-append "^" x)) y))))) ;(car (member "contemplating" mywords (λ (x y) (id? x (car y)))))
;    (and res (cons word (list (cadr res))))))

;(define f (map (λ (x) (symbol->string (list-ref x 2))) freqw)) ;为词频表添加释义
;(define fres (map (λ (x) (findm x)) f)) ;添加释义后的词频表

;搜索整词，rag为返回单词的左右范围 (searchw moby_dick "veil" 5)
(define (searchw art str rag)
  (thread (λ ()
  (let* ((sp (list->vector (string-split (string-downcase art))))
         (len (vector-length sp))
         (key (string-downcase str))
         (art (string-split art)))
    (define (result* rem res)
      (cond ((null? rem) (to-str (reverse res)))
            (else (result* (cdr rem) (cons (cut art (- (car rem) rag) (+ (car rem) rag)) res)))))
    (define (to-str res)
      (set! result (append result (map (λ (x) (apply string-append (map (λ (y) (string-append y " ")) x))) res))))
    (define (iter id rem)
      (cond ((= id len) (result* (reverse rem) '()))
            ((string-contains? (vector-ref sp id) key)
             (iter (+ id 1) (cons id rem)))
            (else (iter (+ id 1) rem))))
    (iter 0 '())))))

;缓存结果集
(define result '())
(define (cls) (set! result '()))
;数据分块　*目前问题是可能把单词切断
(define (cut-str str block)
  (let* ((len (string-length str))
         (part (floor (/ len block))))
    (define (iter start end cnt)
      (if (= cnt block)
          (list (substring str start len))
          (cons (substring str start end)
                (iter end (+ end part) (+ cnt 1)))))
    (iter 0 part 1)))
;线程分发
;cs = (cut-str) ;如果可以反射出函数需要的参数个数就更好
(define (sendto cs fn . para)  
  (cond ((null? cs) 'end)
        (else (and (apply fn `(,(car cs) ,@para))
                   (sendto (cdr cs) fn)))))

;通过(thread-send)来分发线程
;(for ([i 10])
;    (thread-send (search* "Morini literature") (list-ref (cut-str nov2666 10) i)))

;搜索文章，支持以句子为单位的隔词搜索
;example:(search* nov2666 "Morini literature")(search* moby_dick "veil" "[.]" "\"" "[)]")(search* nov17p1 "贞德" "。" "「" "」")
;splt是想要过滤掉的符号，因为通过正则表达式计算，所以特别要注意"."，如果不是默认情况，则必须用"[.]"，这种情况还有"[)]"
;中文搜索，要将分隔符变为"。"
;(how-long search* nov2666 "Morini literature") **如果是多线程程序则无法计算时间
(define (search* art str . splt)
  (thread (λ ()           
    (let* (;(art (thread-receive))
           (sp (if (null? splt)
                  (string-split art ".")
                  (if (= (length splt) 1)
                      (string-split art (car splt))
                      (string-split art (regexp (apply string-append (map (λ (x) (if (eq? (last splt) x) x (string-append x "|"))) splt)))))))) ;生成正则表达式
    (define (iter lart lstr res)
      (cond ((null? lart) (set! result (append result (map (λ (x) (string-filter x "\r" "\n")) res))))
            ((eq? '()
                  (filter (λ (cond) (eq? #f cond))
                          (map (λ (x) (string-contains? (string-downcase (car lart)) (string-downcase x)))
                               lstr)))
             (iter (cdr lart) lstr (cons (string-trim (car lart)) res)))
            (else (iter (cdr lart) lstr res))))
    (iter sp (string-split str) '())))))  ;(iter (string-split (string-filter art "\r" "\n") ".") (string-split str))) ;效率太低

;轮询，在每次循环中询问函数运算线程是否已返回，如果返回则求取该返回值，否则可以进行其他任务而不阻塞
(define (sh fn . args)
  (let ()
    (apply fn args)
    (let loop ()
      (if (null? result)
          (begin
           (writeln "Do something...")
           (loop))
          (car result)))));"Had a result"

;通过正则表达式来搜索文本
;example:(map (λ (s) (regexp-match #px"^[A-Z][\\w]+ing" s)) (string-split (string-filter nov2666 "\r" "\n") "."))
;分析文本中单词间的关系
;单词计数
(define (count art str)
  (let ((set (mkset art)))
    (filter (λ (x) (string=? (car x) str)) set)))

;二分查找
(define (b-search key)
  (let ((len (length mywords)))
    (let loop ((curr (floor (/ len 2)))
               (start 0)
               (end len))
      (if (id? (car (list-ref mywords curr)) key)               
          (list-ref mywords curr)
          (cond ((= curr start) #f)
                ((string<? (car (list-ref mywords curr)) key)
                 (loop (floor (/ (+ curr end) 2)) curr end))
                (else (loop (floor (/ (+ start curr) 2)) start curr)))))))

;为文章中我的生词库中的单词添加注释
;需要增加词组分析，思路是通过改进的(search* art str)准确定位到含有该词组的句子，并在句子末尾添加含义；
(define (analyse art)
  ;(thread (λ ()
  (let (;(art (thread-receive))
         (lt (string-split art)))
    (let/cc skip 'skip
    (define (iter lt res)
      (let* ((word (if (null? lt)
                       (skip (reverse res));(set! result (append result (reverse res))))
                       (string-downcase (car lt))))
             (rem ;(findf (λ (x) ;(and (string-contains? word (symbol->string (car x))) ;此处仍需权衡and/or
                  ;                   (id? word (car x)))
                  ;       mywords)))
              (b-search word)))
        (cond ((not (eq? #f rem))
               (iter (cdr lt) (cons (string-append (car lt) "(" (cadr rem) ")") res)))
              (else (iter (cdr lt) (cons (car lt) res))))))
    (iter lt '()))))

;将字符串列表转换为字符串 ls = (analyse art)
(define (dsp ls)
  (define (iter ls res)
    (cond ((null? ls) (string->symbol res))
          (else (iter (cdr ls) (string-append res " " (car ls))))))
  (iter (cdr ls) (car ls)))

;大小写转换
(define (change-char ch)
  (cond ((and (>= (char->integer ch) 97)
              (<= (char->integer ch) 122))
         (integer->char (+ (char->integer #\A) (- (char->integer ch) (char->integer #\a)))))
        ((and (>= (char->integer ch) 65)
              (<= (char->integer ch) 90))
         (integer->char (+ (char->integer #\a) (- (char->integer ch) (char->integer #\A)))))
        (else "char is not a alphabet!")))

;command line 
(define words
  (command-line
   #:program "words"
   #:once-each
   [("-t" "--test") "Test" (test mywords)]))

;整理
;(file->string v15000)
;(define v15 (filter-not (λ (x) (string=? "" x)) (draw v15000))) ;过滤掉""字符
;(display-lines-to-file v15 "e:/source/words/v15000.txt" #:mode 'text #:exists 'replace)
;从v15000中过滤出简单词汇并输出到simple.txt
;(display-lines-to-file (filter (λ (x) (<= (string-length x) 4)) (map car v15000)) "e:/source/words/simple.txt" #:mode 'text #:exists 'replace)

;过滤出v15000中我的生词表里存在的单词w1
;(define w1 (map (λ (x) (filter (λ (w) (string=? x (car w))) v15000)) (map symbol->string (map car mywords))))
;(define w2 (map symbol->string (map car mywords)))
;过滤出v15000中我的生词表里不存在的单词w3
;(define w3 (filter-not (λ (x) (eq? '() x)) (map (λ (u v) (if (eq? '() u) v '())) w1 w2)))

;用户信息
(struct user (name password) #:mutable)
(struct users (user) #:mutable)
(define utest (users (list (user "Yaoer" "hikari")
                           (user "Yaoer" "hikari")
                           (user "Lianer" "hikari"))))


;struct形式的单词表 
(struct word (spell mean) #:mutable) ;拼写，含义，例句
(struct vocabulary (words) #:mutable)
(define words-list
  (vocabulary
   ;(list ;(word "denote" "代表，表示" "The colour red is used to denote passion or danger.")
         ;(word "aplomb" "自信，沉着，泰然自若" "She performs the duties of a princess with great aplomb.")
    (map (λ (x) (word (car x) (cadr x))) mywords)))

;添加新词 a-word = (word "slightly" "轻微地，稍稍" "I know her slightly.")
(define (insert-word a-word words-list)
  (set-vocabulary-words! words-list (cons a-word (vocabulary-words words-list))))

;通过位置部分修改单词 partofword = set-word-spell! new:string
;example:(modify-word set-word-spell! 0 "denote")
(define (modify-word partofword id new)
  (partofword (list-ref (vocabulary-words words-list) id) new))

;取出所有单词例句 (show-word word-spell)
(define (show-word field)
  (map field (vocabulary-words words-list)))


;xml parser
;(define barron (xml->xexpr (document-element (read-xml (open-input-file "e:/source/words/barron_3500.xml")))))
;(define barron-ls (se-path*/list '(P) barron))
;(define barron+ (apply string-append barron-ls))
;写回html
;(write-xml/content (xexpr->xml `(html (body ((bgcolor "red")) "Hi!" (br) "Bye!"))))
;(xexpr->string '(html (head (title "Hello")) (body "Hi!")))
;将html转换成xexpr
;(xml->xexpr (document-element
;               (read-xml (open-input-file
;                          "e:/source/mywordsweb/visual.html"))))

;目录操作
;(define base "K:\\牛津电子词典")
;(define base-list (directory-list base))
;(define (all-files base base-list)
;  (cond ((null? base-list) '())
;        (else (append (map (λ (x) (build-path base (car base-list) x)) (directory-list (build-path base (car base-list))))
;                      (all-files base (cdr base-list))))))
;(define direct (all-files base base-list))
;(define dict (apply string-append (map file->string direct)))
;(set! dict (string-split dict #rx"\r\n"))
;(display-lines-to-file dict "e:/source/words/dict.txt" #:mode 'text #:exists 'replace)

;将汉字字符串转换为汉字字符表
(define (han-list han)
    (let loop ((len (string-length han))
               (id 1))
      (and (< id len)
           (cons (string-ref han id)
                 (loop len (add1 id))))))

;(define han+ (han-list (force han)))

;构建一个分析动词搭配的函数


;连接MySQL数据库
(define USER "root")
(define PASS "hikari57")
(define mydb   
  (mysql-connect #:user USER
                 #:password PASS
                 #:database "test"))

;将mywords写入数据库中
(define (insert)
  (let ()
    (unless (table-exists? mydb "mywords")
      (query-exec mydb
                  (string-append
                   "CREATE TABLE mywords "
                   "(id INTEGER PRIMARY KEY AUTO_INCREMENT, spell TEXT, mean TEXT)")))    
    (map (λ (x)
           (query-exec mydb
                       "INSERT INTO mywords (spell, mean) VALUES (?, ?)"
                       (car x) (cadr x))) mywords)))

;加入用户信息表
(define (insert-user)
  (let ()
    (unless (table-exists? mydb "users")
      (query-exec mydb
                  (string-append
                   "CREATE TABLE users "
                   "(id INTEGER PRIMARY KEY AUTO_INCREMENT, name TEXT, password TEXT)")))
    (map (λ (x)
           (query-exec mydb
                       "INSERT INTO users (name, password) VALUES (?, ?)"
                       (user-name x) (user-password x)))
                       (users-user utest))))

;核对用户信息 lu = '(name password)
(define (search-u lu)
  (query-rows mydb
              "SELECT name,password FROM users WHERE name = ? AND password = ?"
              (first lu) (second lu)))

;search db  w = "spell"
(define (search-w w)
  (query-rows mydb
              "SELECT spell,mean FROM mywords WHERE spell = ?"
              w))

;(query-rows mydb "select spell,mean from mywords where id = 300");返回值是(list? (vector? res))

;update db word = '("spell" "mean")
(define (update-w word)
  (query-exec mydb
              "UPDATE mywords SET mean = ? WHERE spell = ?"
              (cadr word) (car word)))

;insert db
(define (insert-w word)
  (if (not (eq? '() (search-w (car word))))
      (begin
        (display "已存在该单词，是否更新？y/n ")
        (let ((r (read)))
          (if (eq? r 'y)
              (begin 
                (update-w word)
                (display "更新完成"))
              "放弃更新")))
      (query-exec mydb
                  "INSERT INTO mywords (spell, mean) VALUES (?, ?)"
                  (car word) (cadr word))))

;delete db 
(define (del-w w)
  (query-exec mydb
              "DELETE FROM mywords WHERE spell = ?"
              w))

(provide (all-defined-out))

