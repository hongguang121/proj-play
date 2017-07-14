#lang racket

(require racket/tcp)
(require net/url)
(require net/http-client)
(require net/uri-codec)

;path = "e:/text/"
(define (files path)
  (map (λ (p) (string-append path (path->string p))) (directory-list path)))

;(load-file (files path))
(define (load-file files-path)
  (apply + (map (λ (fp) (length (filter-not (λ (l) (string=? l "")) (file->lines fp)))) files-path))) ;过滤空行
  ;(apply + (map (λ (fp) (length (file->lines fp))) files-path))) ;不过滤空行

(define (insert-html list-page)
  (cond ((null? list-page) '())
        ((string-contains? (car list-page) "shizixq-p1")
         (cons (string-append (substring (car list-page) 0 (- (string-length (car list-page)) 4)) 
                              "\t\t<span><a href=\"../index.html\" style=\"color: #5251d9; padding-left:30px;\">更多老师</a></span></p>")
               (cdr list-page)))
        (else (cons (car list-page)
                    (insert-html (cdr list-page))))))

;(insert-html (file->lines "E:\\ypedp\\html\\ypzk\\szzj\\110.html"))
;(display-lines-to-file (insert-html (file->lines "E:\\ypedp\\html\\ypzk\\szzj\\110.html")) "e:/test.html")

;file-path = (files "e:/html/")
(define (edit-file files-path)
  (map (λ (fp) (display-lines-to-file (insert-html (file->lines fp)) (string-append "e:/html/edit/" (substring fp 8)))) files-path))

;保证7个#\tab
(define (add-tabs str)
  (let loop ((len (string-length str))
             (cnt 0)
             (res ""))
    (cond ((= cnt len) res)
          ((and (> cnt 6) (eq? #\tab (string-ref str cnt)))
           (loop len (add1 cnt) res))
          ((<= cnt 6)
           (loop len (add1 cnt) (string-append res (string #\tab))))
          (else (loop len (add1 cnt) (string-append res (string (string-ref str cnt))))))))

;line: 修改行 flag: 标志 cont: 添加内容
(define (edit-line line flag cont)
  (let loop ((len (string-length line))
             (cnt 0)
             (res ""))
    (cond ((= cnt len) res)
          ((string-contains? res flag)
           (string-append res " " cont (substring line cnt)))
          (else (loop len (add1 cnt) (string-append res (string (string-ref line cnt))))))))

(define (add-class file-path)
  (map (λ (line)
         (if (and (or (string-contains? line "col-sm-4")
                      (string-contains? line "col-sm-3"))
                  (string-contains? line "control-label"))
             (edit-line line "control-label" "key")
             (cond ((and (string-contains? line "form-control")
                         (not (string-contains? line "span")))
                    ;(string-append (substring (add-tabs line) 0 34) " val" (substring line 34))
                    (edit-line line "form-control" "val"))
                   ((string-contains? line "control-label")
                    (edit-line line "control-label" "val"))
                   (else line))))
       (file->lines file-path)))

;(filter-list (list-files (list-dir "F:\\xzfs\\apache-tomcat-8.5.15\\webapps\\fs-mgt\\WEB-INF\\ftl")))
(define (change-bt file-path)
  (map (λ (line)
         (if (and (string-contains? line "btn-flat")
                  (or (string-contains? line "确定")                   
                      (string-contains? line "清空")))
             (edit-line line "btn-flat" "flatten")
             line))
       (file->lines file-path)))

(define (write-file file-path)
  (display-lines-to-file (add-class file-path) file-path #:exists 'replace))

;(map (λ (x) (write-bt-file x)) (filter-list (list-files (list-dir "F:\\xzfs\\apache-tomcat-8.5.15\\webapps\\fs-mgt\\WEB-INF\\ftl"))))
(define (write-bt-file file-path)
  (display-lines-to-file (change-bt file-path) file-path #:exists 'replace))

;"f:/view/order_day_view.ftl"

;"F:\\xzfs\\apache-tomcat-8.5.15\\webapps\\fs-mgt\\res\\js"
;"F:\\xzfs\\apache-tomcat-8.5.15\\webapps\\fs-mgt\\WEB-INF\\ftl"

(define (list-dir dir)
  (map (λ (p) (path->string
               (build-path dir (path->string p))))
       (directory-list dir)))

;dir = (list-dir dir)
;(list-files (list-dir "F:\\xzfs\\apache-tomcat-8.5.15\\webapps\\fs-mgt\\WEB-INF\\ftl"))
(define (list-files dir)
  (flatten (map (λ (d) (map (λ (f) (path->string (build-path d (path->string f)))) (directory-list d))) dir)))

;(filter-list (list-files (list-dir "F:\\xzfs\\apache-tomcat-8.5.15\\webapps\\fs-mgt\\res\\js")))
;(filter-list (list-files (list-dir "F:\\xzfs\\apache-tomcat-8.5.15\\webapps\\fs-mgt\\WEB-INF\\ftl")))
(define (filter-list path-str)
  (filter (λ (s) (string-contains? s "list")) path-str))

(define (edit-js file-path)
  (map (λ (line)
         (cond ((and (string-contains? line "您确定")
                     (= (length (string-split line ".")) 3))
                (string-append
                 (list-ref (string-split line ".") 0)
                 ".html(\"<img src='res/img/wen.png' style='width: 29px;height: 29px;display:inline-block;margin-top: -4px'/>&nbsp;&nbsp;"
                 (substring (list-ref (string-split line ".") 1) 6)
                 "."
                 (list-ref (string-split line ".") 2)))
               ((and (string-contains? line "您确定")
                     (= (length (string-split line ".")) 4))
                (string-append
                 (list-ref (string-split line ".") 0)
                 ".html(\"<img src='res/img/wen.png' style='width: 29px;height: 29px;display:inline-block;margin-top: -4px'/>&nbsp;&nbsp;"
                 (substring (list-ref (string-split line ".") 1) 6)
                 "."
                 (list-ref (string-split line ".") 2)
                 "."
                 (list-ref (string-split line ".") 3)))
               (else line)))
       (file->lines file-path)))

(define (edit-ftl file-path)
  (map (λ (line)
         (cond ((or (string-contains? line "理由")
                    (string-contains? line "备注"))
                (edit-line line "control-label" "reason"))
               ((and (string-contains? line "btn btn-default pull-right")
                     (not (string-contains? line "btn-flat")))
                (if (string-contains? line "取消")
                    (edit-line line "pull-right" "cancels")
                    (edit-line line "pull-right" "sure")))
               (else line)))
       (file->lines file-path)))

(define (write-js-file file-path)
  (display-lines-to-file (edit-js file-path) file-path #:exists 'replace))

(define (write-ftl-file file-path)
  (display-lines-to-file (edit-ftl file-path) file-path #:exists 'replace))

;(filter-not (λ (x) (string-contains? x ".svn")) (list-files (list-dir "F:\\xzfs\\apache-tomcat-8.5.15\\webapps\\fs-mgt\\WEB-INF\\ftl")))

(define (change-button file-path)
  (let ((ls (file->lines file-path)))
    (let loop ((ls ls)
               (res '())
               (rem '()))
      (cond ((null? ls) (reverse res))
            ((and (string-contains? (car ls) "cancels")
                  (string-contains? (car ls) "取消"))
             (loop (cdr ls) res (car ls)))
            ((and (string-contains? (car ls) "sure")
                  (string-contains? (car ls) "确定"))
             (loop (cdr ls)
                   (cons rem
                         (cons (car ls) res))
                   '()))
            (else (loop (cdr ls) (cons (car ls) res) rem))))))

(define (write-ftlcb-file file-path)
  (display-lines-to-file (change-button file-path) file-path #:exists 'replace))


(define (edit-alert-confirm-js file-path)
  (map (λ (line)
         (cond ((and (string-contains? line "bootbox.alert")
                     (= (length (string-split line "(")) 2))                                    
                (string-append
                 (list-ref (string-split line "(") 0)
                 "(\"<img src='res/img/tan.png' style='width: 29px;height: 29px;margin-top: -4px'>&nbsp;&nbsp;\" + "
                 (list-ref (string-split line "(") 1)))
               ((and (string-contains? line "bootbox.alert")
                     (= (length (string-split line "(")) 3))
                (string-append
                 (list-ref (string-split line "(") 0)
                 "(\"<img src='res/img/tan.png' style='width: 29px;height: 29px;margin-top: -4px'>&nbsp;&nbsp;\" + "
                 (list-ref (string-split line "(") 1)
                 "("
                 (list-ref (string-split line "(") 2)))
               ((string-contains? line "bootbox.confirm")                 
                (string-append
                 (list-ref (string-split line "(") 0)
                 "(\"<img src='res/img/wen.png' style='width: 29px;height: 29px;margin-top: -4px'>&nbsp;&nbsp;\" + "
                 (list-ref (string-split line "(") 1)
                 "("
                 (list-ref (string-split line "(") 2)))
               (else line)))
       (file->lines file-path)))

(define (write-alert-confirm-file file-path)
  (display-lines-to-file (edit-alert-confirm-js file-path) file-path #:exists 'replace))