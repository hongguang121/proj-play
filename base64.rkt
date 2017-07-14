#lang racket

(require net/base64)
(require 2htdp/image
         racket/cmdline
         (only-in racket/draw read-bitmap))

(define dc (base64-decode (file->bytes "e:/b64.txt")))

(define ec (base64-encode dc))

;(read-bitmap (base64-decode (file->bytes "e:/base64")))