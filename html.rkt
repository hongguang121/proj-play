#lang racket
(require html-parsing)
(require html-writing)
(require html-template)

(require html)
(require html)
(require xml
         xml/path)

(define poem (file->string "E:\\Source\\poem\\poem.txt"))

(define poem-html-parsing (html->xexp poem))
(define poem-writing-html (write-html poem-html-parsing (current-output-port)))



