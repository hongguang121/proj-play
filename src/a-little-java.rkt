#lang racket

(define (point city x y)
  (define (Cartesian-pt)
    (sqrt (+ (expt x 2) (expt y 2))))
  (define (Manhattan-pt)
    (+ x y))
  (case ((λ () city))
    ((Cartesian-pt) (Cartesian-pt))
    ((Manhattan-pt) (Manhattan-pt))))

(printf "Manhattan-pt's distance: ~a\n" (point 'Manhattan-pt 3 4))
(printf "Cartesian-pt's distance: ~a\n" (point 'Cartesian-pt 3 4))

(define shish-list '(onion lamb tomato))

(define (only-onion shish-list)
  (cond ((null? shish-list) #t)
        ((not (eq? 'onion (car shish-list))) #f)
        (else (only-onion (cdr shish-list)))))

(define meza '(Shrimp Calamari Escargots Hummus))
(define main '(Steak Ravioli Chicken Eggplant))
(define salad '(Green Cucumber Greek))
(define dessert '(Sundae Mousse Torte))
(struct dish (name vegetarian? calories type))
(define dish-list (list
                   (dish 'pork #f 800 'meat)
                   (dish 'beef #f 700 'meat)
                   (dish 'chicken #f 400 'meat)
                   (dish 'french-fries #t 530 'other)
                   (dish 'rice #t 350 'other)
                   (dish 'season-fruit #t 120 'other)
                   (dish 'pizza #t 550 'other)
                   (dish 'prawns #f 300 'fish)
                   (dish 'salmon #f 450 'fish)))

(define menu '(Shrimp Torte Chicken Green Sundae Escargots Eggplant Greek))

;exp:过滤出dish-list中calories>300的条目的前三个的名字
(take (map dish-name (filter (λ (x) (> (dish-calories x) 300)) dish-list)) 3)
(map dish-name (filter (λ (x) (dish-vegetarian? x)) dish-list))

(define (add-steak x)
  (cons x '(Steak)))

(add-steak 'Ravioli)

(define (return-type x)  
  (cond ((not (eq? #f (member x meza))) 'meza)
        ((not (eq? #f (member x main))) 'main)
        ((not (eq? #f (member x salad))) 'salad)
        ((not (eq? #f (member x dessert))) 'dessert)
        (else "unknown type")))

;返回元素集的类型集合 ls = menu
(define (type-set ls)
  (let loop ((set '())
             (ls ls))
    (cond ((null? ls) set)
          ((eq? #f (member (return-type (car ls)) set))
           (loop (cons (return-type (car ls)) set) (cdr ls)))
          (else (loop set (cdr ls))))))

;按类型分配元素
(define (grouping-type ls)
  (let loop ((type (type-set ls)))
    (cond ((null? type) '())
          (else (cons (list (car type)
                            (filter (λ (t) (eq? (car type) (return-type t))) ls))
                      (loop (cdr type)))))))

;检查ls中是否有T类型，T是该类型的集合
(define (has-type? ls T)
  (cond ((null? ls) #f)
        ((member (car ls) T) #t)
        (else (has-type? (cdr ls) T))))

(define (add-type ls)
  (map (λ (x) (cons x (return-type x))) ls))

(define (check-class ls T)
  (cond ((null? ls) '())
        ((not (member (car ls) T))
         (printf "Type error: ~a\n" (car ls)))
        (else (check-class (cdr ls) T))))

(has-type? '(Shrimp Ravioli Green) dessert)
(has-type? '(Shrimp Ravioli Green) salad)

(map return-type '(Shrimp Ravioli Green))
(check-class '(Shrimp Ravioli Escargots) meza)

;Apple class '(color weight)
(define apple '(("red" 150) ("green" 164) ("red" 135)))

(define (apple-color app)
  (car app))

(define (apple-weight app)
  (cadr app))

(filter (λ (app) (string=? "red" (apple-color app))) apple)
(filter (λ (app) (>= (apple-weight app) 150)) apple)
(sort apple #:key cadr >=)

(define ls '(1 2 3))
(define lt '(3 4))

(define (cross ls lt)
    (let loop ((la ls)
               (lb lt))
      (cond ((null? lb)
             (loop (cdr la) lt))
            ((null? la)
             '())
            (else (cons (list (car la) (car lb))
                        (loop la (cdr lb)))))))

(define c (apply append (map (λ (x) (map (λ (y) (list x y)) lt)) ls)))
(filter (λ (x) (= (remainder (apply + x) 3) 0)) c)