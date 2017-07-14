#lang racket/gui

(define (square n) (* n n))

(define frame (new frame%
                   [label "Example"]
                   [width 800]
                   [height 600]))

(define msg (new message% [parent frame]
                          [label "No events so far..."]
                          [vert-margin 30]
                          [horiz-margin 30]))

(new button% [parent frame]
             [label "Click Me"]
             ; Callback procedure for a button click:
             [callback (lambda (button event)
                         (send msg set-label "Button click"))])

(new button% [parent frame]
             [label "Pause"]
             [callback (lambda (button event) (sleep 5))])

(new button% [parent frame]
             [label "Square"]
             [callback (lambda (button event)
                         (send msg set-label (number->string (square 5))))])

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

(new my-canvas% [parent frame])

(define panel (new horizontal-panel% [parent frame]))
(new button% [parent panel]
             [label "Left"]
             [callback (lambda (button event)
                         (send msg set-label "Left click"))])
(new button% [parent panel]
             [label "Right"]
             [callback (lambda (button event)
                         (send msg set-label "Right click"))])

(new canvas% [parent frame]
             [paint-callback
              (lambda (canvas dc)
                (send dc set-scale 3 3)
                (send dc set-text-foreground "blue")
                (send dc draw-text "Don't Panic!" 0 0))])

; Create a dialog
(define dialog (instantiate dialog% ("Example")))
 
; Add a text field to the dialog
(new text-field% [parent dialog] [label "Your name"])
 
; Add a horizontal panel to the dialog, with centering for buttons
(define panel+ (new horizontal-panel% [parent dialog]
                                     [alignment '(center center)]))
 
; Add Cancel and Ok buttons to the horizontal panel
(new button% [parent panel+] [label "Cancel"])
(new button% [parent panel+] [label "Ok"])
(when (system-position-ok-before-cancel?)
  (send panel+ change-children reverse))

(send dialog show #t)
(send frame show #t)