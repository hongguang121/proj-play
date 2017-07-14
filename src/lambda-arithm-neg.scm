; Subtraction, division, and negative numbers in lambda-calculus
;
; This file will demonstrate two ad hoc and two systematic ways of
; implementing a predecessor function. The systematic approaches
; introduce negative numbers. One of the techniques is more
; theoretical; the other leads to rather practical operations on
; arbitrary integers. That approach permits a straightforward
; definition of subtraction and division of two numerals. Curiously,
; none of the arithmetic operations involve the Y combinator; even the
; division gets by without Y.

; $Id: lambda-arithm-neg.scm,v 2.1 2001/05/30 04:04:16 oleg Exp oleg $

; Include booleans, basic combinators, Church numerals, and the
; definitions of addition, multiplication and exponentiation.

(X include "lambda-arithm-basic.scm")

;------------------------------------------------------------------------
;			A predecessor function
; A Church numeral is an iterator. A natural number is represented by
; the count of applying a function. A successor therefore is trivial:
; it performs one more application, on the result of cn. This line of
; reasoning makes the definition of a predecessor hard: given cn, we
; need to _unapply_ a function.

; One ad hoc way of defining a predecessor of a positive numeral
;		%predp cn+1 ==> cn
; is to represent "%predp cn" as "cn f v"
; where f and v are so chosen that (f z) acts as
;	if z == v then c0 else (succ z)
; We know that z can be either a numeral cn or a special value v. All
; Church numerals have a property that (cn %I) is %I: the identity
; combinator is a fixpoint of every numeral. Therefore, ((cn %I) (succ
; cn)) reduces to (succ cn). We only need to choose the value v in such
; a way that ((v I) (succ v)) yields c0.

(X Define* %predp 
   (L c (c 
	 (L z (z %I (%succ z)))	; function f(z)
	 (L a (L x %c0)))))	; value v

(%predp %c1) ;==> %c0
(X equal?* (%predp %c1) %c0) ;==> #t
(%predp %c3) ;==> which is %c2
(X equal?* (%predp %c3) %c2) ;==> #t

; Comparison of two Church numerals, a and b
; To see if numeral a is greater than numeral b we apply a
; stopping-at-zero predecessor to 'a', b times. If the result is
; non-zero, a is indeed larger than b. A faster and a more memory
; efficient algorithm is to have numeral 'a' build a list and then get
; numeral 'b' to destroy it, cell by cell. If there is something left,
; 'a' is greater than 'b'.

; Numeral a will be building a list
; (%cons %true (%cons %true .... (cons %false %false)))
; where there are as many %true terms as there is the cardinality of a

(X Define* %greater?
   (L a (L b
      ; Checking if anything is left of the list: if the current
      ; cell is nil
     (%car
      (b
       ; deconstructing the list. Once we hit the nil, we stop
       (L z (%if (%car z) (%cdr z) z))
       ; Building the list
       (a
	(L z (%cons %true z))
	(%cons %false %false)) ; Initial cell, the nil
)))))

(X set-eval-threshold 1000)

(%greater? %c1 %c0) ;==> %true
(%greater? %c0 %c0) ;==> %false
(%greater? %c2 %c3) ;==> %false
(%greater? %c3 %c1) ;==> %true


; Another ad hoc way of defining a predecessor is a more explicit
; variation of the above approach. Recall that "%predp cn f x" is to
; apply f to x (n-1) times. Also recall that "cn g u" results in an
; application of g to u n times. If a function g can tell the first
; application from the others, we can indeed apply f to x (n-1) times.
; Therefore we make the value u to be a pair, with the first element
; being a flag. (g u) is a pair as well, whose first element however
; is set to %false.

(X Define %pred1p
   (L m (L f (L x
       (%cdr
	(m (L u (%if (%car u)
		     (%cons %false x)
		     (%cons %false (f (%cdr u))) ))
	   (%cons %true %false)))))))

(X equal?* (%pred1p %c1) %c0) ;==> #t
(X set-eval-threshold 1000)
(X equal?* (%pred1p %c2) %c1) ;==> #t


; In the definition of %pred1p above, the value u should be more
; properly set as (%cons %true term-M) rather than (%cons %true
; %false).  The term-M is a free variable. The result of applying
; %pred1p to any non-zero Church numeral does not depend on the choice
; for term-M. It is only when %pred1p is applied to %c0 that the
; meaning of term-M is revealed:

; (%pred1p %c0) ;==> (L f (L x term-M))
; Therefore, term-M must be (-1 f x)!

;------------------------------------------------------------------------
;		Negative numbers and -1: a recursive way
;
; If we can define a term representing -1, we can approach the
; predecessor function in a systematic way, as an addition of -1 to a
; number. We can also introduce negative numbers as a product of a
; Church numeral and -1.
;
; -1 can be defined as a fixpoint of a suitably chosen function
; (abstraction). It is easy to see that the equation
;	b + b + 1 = b
; has only one solution, namely, minus one. Therefore, we are strongly
; tempted to define -1 as
;; (X Define* %-1-e (L b (%succ (%add b b))))
;; or, in an unadorned way,
;; (X expand-shortcuts %-1-e)
;; ; ==> (L b (L f (L x (f ((b f) ((b f) x))))))
;; (X Define %-1 (%Y %-1-e))
;; (X set-eval-threshold 10)
;; (%-1 f (f x))


;------------------------------------------------------------------------
;		Negative numbers and -1: a practical way
;   Addition, Subtraction, Multiplication and Division of Integers
;
; Note that neither of operations below make any use of a fixpoint
; combinator.
;
; We define integer numbers as a sign-bit abstraction over natural
; numbers  (Church numerals):
; Non-negative integers %in are defined as (L sign-bit %cn); 
; negative integers %i-n are defined as
;	(L sign-bit (L f (L x (sign-bit (%cn f x)))))
; A non-negative integer disregards the sign term; negative integers
; apply the sign term to the result of a numeral

(X Define* %i0 (L sign %c0))	; A zero integer
(X Define* %i1 (L sign %c1))	; An integer +1
(X Define* %i-1 (L sign (L f (L x (sign (f x))))))	; -1

; c->i: N->Z
; Convert a natural number to an integer
(X Define* %c->i (L c (L sign c)))

; abs: Z -> N
; finds the magnitude of an integer Z, which is a natural number
; abs is defined by setting the identity function as a
; sign term. In that case, it doesn't matter if it is applied or not.
(X Define* %abs (L i (i %I)))

; Verifying %abs function
(%abs %i0) ;==> %c0
(%abs %i1) ;==> %c1
(%abs %i-1) ;==> (L f f)
(X equal?* (%abs %i-1) %c1) ; ==> #t

; Testing if an integer is positive or negative

; sign-case: Z -> on-positive -> on-zero -> on-negative -> term
; This function takes a number and three terms and returns one of them
; depending on the sign of the number.
; The branching is an application of the integer to three suitably-chosen
; terms. The underscore symbol '_' in is a regular,
; undistinguished identifier.

(X Define* %sign-case
   (L i (L on-positive (L on-zero (L on-negative
      (i
        (L _ on-negative)
	(L _ on-positive)
	on-zero))))))

; negative?: Z -> boolean
(X Define* %negative?
   (L i (%sign-case i %false %false %true)))
(%negative? %i0) ;==> %false
(%negative? %i1) ;==> %false
(%negative? %i-1) ;==> %true

; c-negate: N -> Z
; Given a non-zero natural number, turn it into a negative integer
(X Define* %c-negate (L m (L sign (L f (L x (sign (m f x)))))))


; i-negate: Z -> Z
; Negate an integer
; The lazy semantics inherent in the normal order really helps us here
(X Define* %i-negate
   (L i (%sign-case i (%c-negate (%abs i)) %i0 (%c->i (%abs i)))))


; The successor of an integer number a
(X Define* %i-succ
   (L a
      (%sign-case a
       ; if the number is positive
       (%c->i (%succ (%abs a)))
       ; if the number is zero
       %i1
       ; if the number is negative
       (%i-negate
	(%c->i (%predp (%abs a)))))))


(%i-succ %i0) ; ==> %i1
(%i-succ %i1) ; ==> (L sign (L f (L x (f (f x)))))
(X equal?* (%i-succ %i1) (L n %c2)) ;=> #t
(%i-succ %i-1) ;=> %i0
(X equal?* (%i-succ (%c-negate %c2)) %i-1) ;=> #t


; Addition of two integer numbers, a and b
(X Define* %i-add
  (L a (L b
    (%sign-case a
       ; a > 0
       (%sign-case b
	 ; b > 0
	   (%c->i (%add (%abs a) (%abs b)))
	 ; b = 0
	   a
	 ; b < 0: increment b a times
	   ((%abs a) %i-succ b)
	   )
       ; a = 0
       b
       ; a < 0
       (%sign-case b
	 ; b > 0: increment a b times
	   ((%abs b) %i-succ a)
	 ; b = 0
	   a
	 ; b < 0
	   (%c-negate (%add (%abs a) (%abs b))))
))))


(X set-eval-threshold 100000)
(X equal?* (%i-add %i0 %i0) %i0) ;==> #t
(%i-add %i0 %i1) ;==> %i1
(X equal?* (%i-add %i0 %i1) %i1) ;==> #t
(%i-add %i1 %i0) ;==> %i1
(X equal?* (%i-add %i1 %i0) %i1) ;==> #t
(%i-add %i1 %i-1) ;==> %i0
(X equal?* (%i-add %i1 %i-1) %i0) ;==> #t
(%i-add (%c-negate %c2) %i-1) ;==> (L sign (L f (L x (sign (f (f (f x)))))))
(X equal?* (%i-add (%c-negate %c2) %i-1) (%c-negate %c3))
(%i-add (%c-negate %c2) %i1) ;==> (L sign (L f (L x (sign (f x))))), i.e, %i-1
(X equal?* (%i-add (%c-negate %c2) %i1) %i-1) ;==> #t
(%i-add (%c->i %c2) %i-1) ;==> %i1
(X equal?* (%i-add (%c->i %c2) %i-1) %i1)
(%i-add (L n %c2) %i1) ;==> (L sign (L f (L x (f (f (f x)))))), which is %i3
(X equal?* (%i-add (L n %c2) %i1) (L n %c3))


; Subtraction of two integers
(X Define* %i-sub
   (L a (L b (%i-add a (%i-negate b)))))

;(X expand-shortcuts %i-sub)

(%i-sub %i1 %i1) ;==> %i0
(%i-sub (%c->i %c2) %i1) ;==> %i1
(%i-sub %i1 (%c->i %c2)) ;==> (L sign (L f (L x (sign (f x)))))
(%i-sub %i-1 %i0) ;==> %i-1
(%i-sub %i0 %i-1) ;==> (L sign (L f f)), which is %i1
(X equal?* (%i-sub %i0 %i-1) %i1) ;==> #t
; 2 - 3 => -1
(%i-sub (%c->i %c2) (%c->i %c3)) ;==> (L sign (L f (L x (sign (f x)))))
; 3 - 2 => 1
(%i-sub (%c->i %c3) (%c->i %c2)) ;==> %i1
(%i-sub %i-1 %i-1) ;==> %i0
(X equal?* (%i-sub (%c-negate %c2) %i1) (%c-negate %c3)) ;==> #t
(X equal?* (%i-sub %i1 (%c-negate %c2)) (%c->i %c3)) ;==> #t
(%i-sub %i-1 (%c-negate %c2)) ;==> %i1
(X equal?* (%i-sub %i-1 (%c-negate %c2)) %i1) ;==> #t
(X equal?* (%i-sub (%c-negate %c2) %i-1) %i-1) ;==> #t

; Multiplication of two integers
(X Define* %i-mul
  (L a (L b
   ( ; determine the sign of the result
    (%sign-case a
       ; a > 0
       (%sign-case b
	 (L x (%c->i x)) (L _ %i0) (L x (%c-negate x)))
       ; a = 0
       (L _ %i0)
       ; a < 0
       (%sign-case b
	 (L x (%c-negate x)) (L _ %i0) (L x (%c->i x))))
    (%mul (%abs a) (%abs b))))))

(%i-mul %i0 %i1) ;==> %i0
(%i-mul %i0 %i-1) ;==> %i0
(%i-mul %i-1 %i0) ;==> %i0
; 2*3 => 6
(X equal?* (%i-mul (%c->i %c2) (%c->i %c3)) (%c->i (%add %c2 (%add %c2 %c2)))) ;==>#t
(X equal?* (%i-mul (%c-negate %c2) %i1) (%c-negate %c2)) ;==>#t
(X equal?* (%i-mul (%c-negate %c2) %i-1) (%c->i %c2)) ;==>#t
(X equal?* (%i-mul (%c-negate %c2) (%c-negate %c3)) (%c->i (%add %c2 (%add %c2 %c2)))) ;==>#t


; Division of two integers

; Division is slightly more involved. As in the case of multiplication,
; we analyze the signs of the operands, determine the sign of the result
; and then do division of two positive numbers. The algorithm is
; classical: keep subtracting the divisor from the dividend until the
; result becomes less than the divisor. Count the number of subtractions
; performed. The final count is the quotient. In Haskell terms,
; 	mydiv dividend divisor = if dividend < divisor
; 		then 0
; 		else 1 + (mydiv (dividend - divisor) divisor)
; This algorithm is behind the arithmetics of types,
; type-arithmetics.html.  However it relies on recursion, which we
; would like to avoid.  It is easy to see that, given positive divisor
; and dividend, we need to do at most dividend subtractions. We can do
; exactly dividend iterations if we keep a flag that tells us that we
; have done enough subtractions already. To be slightly more formal:
;
; mydiv dividend divisor =
;       let div_iter (current, flag, accumulator) =
; 	           if flag || (current < divisor)
; 	           then (current, True, accumulator) -- enough subtractions
; 	           else (current-divisor, False, 1+accumulator) -- continue
;       in
;       case  ntimes dividend div_iter (dividend,False,0) of
; 	    (_,_,accum) -> accum
;       where ntimes n =		-- Church Numeral n
; 	      case n of 0 -> (\f seed -> seed)
; 		        n -> (\f seed -> ntimes (n-1) f (f seed))


(X Define* %i-div
  (L a (L b
   ( ; determine the sign of the result
    (%sign-case a
       ; a > 0
       (%sign-case b
	 (L x (%c->i x)) (L _ %true) (L x (%c-negate x)))
       ; a = 0
       (L _ %i0)
       ; a < 0
       (%sign-case b
	 (L x (%c-negate x)) (L _ %true) (L x (%c->i x))))
    ; now divide x by y, where x and y are two positive integers
    ((L x (L y
     (%cdr (%cdr
      (x		; the dividend drives the iteration
       %I
       (L u
	  ((L current (L flag (L accum
	      (%if flag u  ; we're done
		   ((L diff		 ; current - y
		       (%sign-case diff
					; diff > 0
				   (%cons diff (%cons %false (%succ accum)))
					; diff = 0
				   (%cons diff (%cons %true (%succ accum)))
					; diff < 0
				   (%cons diff (%cons %true accum))))
		    (%i-sub current y))))))
	   (%car u) (%car (%cdr u)) (%cdr (%cdr u))))
       (%cons x (%cons %false %c0)))))))
     (%c->i (%abs a)) (%c->i (%abs b)))))))

; A few tests
(%i-div %i1 %i1) ;==> (L sign (L f f))
(%i-div (%c->i %c2) %i1) ;==> (L sign (L f (L x (f (f x)))))
(X equal?* (%i-div (%c->i %c2) %i1) (%c->i %c2)) ;==> #t
(%i-div (%c->i %c2) %i-1) ;==> (L sign (L f (L x (sign (f (f x)))))), i.e., -2
(X equal?* (%i-div (%c->i %c2) %i-1) (%c-negate %c2)) ;==> #t
; 5 / 2 ==> 2
;(%i-div (%c->i (%succ (%add %c2 %c2))) (%c->i %c2))
;(X equal?* (%i-div (%c->i (%succ (%add %c2 %c2))) (%c->i %c2)) (%c->i 2));==>#t
; -3/2 => -1
(X equal?* (%i-div (%c-negate %c3) (%c->i %c2)) %i-1) ;==> #t
; -3/-2 => 1
(X equal?* (%i-div (%c-negate %c3) (%c-negate %c2)) %i-1) ;==> #t
(%i-div %i0 (%c->i %c2)) ;==> %i0
(%i-div (%c->i %c2) (%c->i %c3)) ;==> (L sign %c0)

; ==> (L sign (L f (L x (f (f x)))))
;(%i-div (%i-succ (%i-mul (%i-add %i1 %i1) (%i-add %i1 %i1))) (%i-add %i1 %i1))
