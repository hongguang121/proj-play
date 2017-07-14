#lang racket

((λ (a b) (+ a b)) 1 2)

(((λ (y) (λ (x) (* x y))) 3) 4)

(let ((m 5))
    ((λ (n) (+ n m)) 4))

(let ((m 5)(n 3))
    ((λ (n) (+ n m)) 4))

(let ((m 5)(n 3))
    (λ (n) (+ n m)))

#|
var Y = function (le){
	return function (f){
		return f(f);
	} (function (f){
		return le(function(x){
			return (f(f))(x);
		});
	});
};
|#

#| javascript lexical scope
var count = () => {
... var res = [];
... for (var i = 0; i < 5; i ++){
..... res.push( () => { return i * i; } ) }  //js中变量是可变的，所以作用域中i的值在每一次循环中都被修改一次，所以i的最终值将是循环结束后i的值（只有一个i）
... return res; }

eval(add.toString());

var lst = count();

lst[0]() => 25
.
.
.
lst[4]() => 25

var iter = () => {
... for (var i = 0; i < 5; i ++){
..... console.log( ( () => { return i * i; } ) () ) } //这里直接进行了求值
... }

|#

(define rem '())

(let loop ((i 0))
  (unless (= i 5)
    ;;Scheme中变量是不可变的，所以每一次i都是一个新的作用域，产生i的一个新的副本，而procedure则被应用到每个不同的i上，它们彼此独立，不互相干涉
    (set! rem (cons (λ () (* i i)) rem)) 
    (loop (add1 i))))

((list-ref rem 0))

(define lazy '())

(let loop ((i 0))
  (unless (= i 5)
    (set! lazy (cons (delay (λ (i) (* i i))) rem))
    (loop (add1 i))))

((force (list-ref lazy 0)) 9)


