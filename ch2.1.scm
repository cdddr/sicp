`4;;; Introduction to Data Abstraction

;;; Data abstraction is a methodology that enables us to isolate how a compound data object is used from the details
;;; of how it is constructed from more primitive data objects.
;;;
;;; Programs should use data in such a way as to make no assumptions about the data that are not strictly necessary
;;; for performing the task at hand. The corralary to this is that the concrete data representation should be
;;; defined independent of the programs that are using the data.

;;; Section 2.1.1 - Arithmetic Operations for Rational Numbers

;;; Constructor and Selector spec
;;;	(make-rat <n> <d>) returns the rational number whose numerator is the integer <n> and whose denominator
;;;			   is the integer <d>.
;;;	(numer <x>) returns the numerator of the rational number x
;;;	(denom <x>) returns the denominator of the rational number x

;;; Expressing operations on rational numbers
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
	    (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
	    (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

;;; Now, we need to glue together a numerator and denominator to form a rational, ie define make-rat. To accomplish
;;; this we will utilize pairs using cons, car, and cdr.
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
;; Rv 1 : Revised to reduce terms
(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

;;; Results
;; racket@> (define one-half (make-rat 1 2))
;; racket@> (print-rat one-half)

;; 1/2racket@> (define one-third (make-rat 1 3))
;; racket@> (print-rat (add-rat one-half one-third))

;; 5/6racket@> (print-rat (mul-rat one-half one-third))

;; 1/6racket@> (print-rat (add-rat one-third one-third))

;; 6/9racket@> <---- does not reduce terms (obviously).
;; racket@> (print-rat (add-rat one-third one-third))

;; 2/3racket@> <---- after revising make-rat to use gcd, it reduces.

;; Rv 2 : Revised to handle negative terms
(define (make-rat n d)
  (let ((g (abs (gcd n d)))
	(pair (cond ((or (and (> n 0) (< d 0)) (and (< n 0) (< d 0)))
		     (cons (* n -1) (* d -1)))
		    (else
		     (cons n d)))))
    (cons (/ (numer pair) g) (/ (denom pair) g))))

;;; Results
;; racket@> (print-rat (make-rat 2 -1))

;; -2/1racket@> 
;; racket@> (print-rat (make-rat -2 -1))

;; 2/1racket@> 
;; racket@> (print-rat (make-rat -2 1))

;; -2/1racket@> 
;; racket@> (print-rat (make-rat 2 1))

;; 2/1racket@>

;; Rv 3 : Get rid of the intermediate data structure and let the division fix the signs.
(define (make-rat n d)
  (let ((g ((if (< d 0) - +) (abs (gcd n d)))))
    (cons (/ n g) (/ d g))))

;;; Results
;; racket@> (print-rat (make-rat 2 1))

;; 2/1racket@> 
;; racket@> (print-rat (make-rat 2 -1))

;; -2/1racket@> 
;; racket@> (print-rat (make-rat -2 1))

;; -2/1racket@> 
;; racket@> (print-rat (make-rat -2 -1))

;; 2/1racket@> 


;;; Exercise 2.2
(define (make-segment p1 p2)
  (cons p1 p2))

(define (start-segment seg)
  (car seg))

(define (end-segment seg)
  (cdr seg))

(define (make-point x y)		;some opportunity for abstraction here
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (midpoint-segment s)
  (let ((x1 (x-point (start-segment s)))
	(x2 (x-point (end-segment s)))
	(y1 (y-point (start-segment s)))
	(y2 (y-point (end-segment s))))
    (make-point (/ (+ x1 x2) 2) (/ (+ y1 y2) 2))))

;; racket@> (print-point (midpoint-segment (make-segment (make-point 0 0) (make-point 1 1))))
;; (1/2,1/2)racket@>

;;; Exercise 2.3 - Rectangles in a plane
;;;   Four Segments based on Four points
;;;  p4 +-----------+ p3
;;;     |           |
;;;     |           | s2
;;;     |           |
;;;  p1 +-----------+ p2
;;;           s1
;;; In terms of the procedures needed for manipulation, only need to know about 2 sides
(define (make-rect s1 s2)
  (cons s1 s2))

(define (segment-length s)
  (let ((x1 (x-point (start-segment s)))
	(y1 (y-point (start-segment s)))
	(x2 (x-point (end-segment s)))
	(y2 (y-point (end-segment s))))
    (sqrt (+ (expt (- x2 x1) 2) (expt (- y2 y1) 2)))))

(define (rect-side1 r)
  (segment-length (car r)))
(define (rect-side2 r)
  (segment-length (cdr r)))

;;; Perimeter Procedure
(define (rect-perimeter r)
  (+ (* 2 (rect-side1 r)) (* 2 (rect-side2 r))))

(define (rect-area r)
  (* (rect-side1 r) (rect-side2 r)))

;; racket@> (rect-area (make-rect (make-segment (make-point 0 0) (make-point 2 0))
;; 			       (make-segment (make-point 2 0) (make-point 2 4))))
;; 8
;; racket@> (rect-perimeter (make-rect (make-segment (make-point 0 0) (make-point 2 0))
;; 			       (make-segment (make-point 2 0) (make-point 2 4))))
;; 12

;;; Different representation - a set of four points
(define (make-rect p1 p2 p3 p4)
  (cons (segment-length (make-segment p1 p2))
	(segment-length (make-segment p2 p3))))
(define (rect-side1 r)
  (car r))
(define (rect-side2 r)
  (cdr r))

;; racket@> (rect-area (make-rect (make-point 0 0) (make-point 2 0)
;; 			       (make-point 2 4) (make-point 0 4)))
;; 8
;; racket@> (rect-perimeter
;; 	  (make-rect (make-point 0 0) (make-point 2 0)
;; 			       (make-point 2 4) (make-point 0 4)))
;; 12

;;; Exercise 2.4
(define (ex2.4-cons x y)
  (lambda (m) (m x y)))

(define (ex2.4-car z)
  (z (lambda (p q) p)))

(define (ex2.4-cdr z)
  (z (lambda (p q) q)))

;; racket@> (ex2.4-car (ex2.4-cons 1 2))
;; 1
;; racket@> (ex2.4-cdr (ex2.4-cons 1 2))
;; 2

;;; Exercise 2.5 - representing pairs of integers with a single number : 2^a*3^b
(define (ex2.5-log b x)
  (/ (log x) (log b)))

(define (ex2.5-cons x y)
  (* (expt 2 x) (expt 3 y)))

(define (ex2.5-iter op n d result)
    (cond ((and (= d 3) (not (= (modulo result d) 0)))
	   (cond ((= op 0) (ex2.5-iter op 0 2 result))
		 ((= op 1) n)))
	  ((and (= d 2) (not (= (modulo result d) 0)))
	   n)
	  (else
	   (ex2.5-iter op (+ n 1) d (/ result d)))))

(define (ex2.5-car z)
  (ex2.5-iter 0 0 3 z))

(define (ex2.5-cdr z)
  (ex2.5-iter 1 0 3 z))

;; racket@> (ex2.5-car (ex2.5-cons 4 1003))
;; 4
;; racket@> (ex2.5-cdr (ex2.5-cons 4 1003))
;; 1003

;;; the above is kind of convoluted from a logical standpoint...is there a simpler way? gcd?
;;; The result is an even number multiplied by an odd. use gcd on subsequent divisions and count?
(define (ex2.5-iter1 n d result)
  (if (= (gcd d result) 1)
      n
      (ex2.5-iter1 (+ n 1) d (/ result d))))

(define (ex2.5-car1 z)
  (ex2.5-iter1 0 2 z))

(define (ex2.5-cdr1 z)
  (ex2.5-iter1 0 3 z))

;; racket@> (ex2.5-car1 (ex2.5-cons 1000 10003))
;; 1000
;; racket@> (ex2.5-cdr1 (ex2.5-cons 1000 10003))
;; 10003

;; This seems like a terrible way to actually store pairs from a memory storage perpspective, but was a neat exercise.

;;; Exercise 2.6 - Church numerals - this is f'in crazy.

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

;; Defining one : substitute zero into (add-1 zero)
;; (add-1 (lambda (f) (lambda (x) x)))
;; (lambda (f)
;;   (lambda (x)
;;     (f ((lambda (g)
;; 	  (lambda (y) y) f)
;; 	x))))

;; ((lambda (g)			
;;    (lambda (y) y)) f)  ---> (should return f)

;; This means that the original substitution simplifies a bit:
;; (lambda (f)
;;   (lambda (x)
;;     (f ((lambda (y) y) x))))

;; (lambda (f)
;;   (lambda (x)
;;     (f x)))

;;; using the above reasoning
(define one (lambda (f) (lambda (x) (f x))))

;; Defining two : substitute one into (add-1 one)
;; (lambda (f) (lambda (x)
;; 	      (f ((one f) x))))
;; (lambda (f) (lambda (x)
;; 	      (f ((lambda (x) (f x)) x) )))
;; (lambda (f) (lambda (x)
;; 	      (f (f x))))

(define two (lambda (f) (lambda (x) (f (f x)))))

;; Addition looks like repeated applications of f to x, adding a to b is (a+b) applications of f to zero (x)
;;; a itself is going to be a applications of f to x
;;; b will also be b applications of f to x. So, we can pass the results of (b f) to (a f) with x .........
(define (add a b)
  (lambda (f)
    (lambda (x)
      ((a f) ((b f) x)))))

;; .... i think, hey it works! These hurt my brain.
;; racket@> ((one (lambda (n) (+ n 1))) 0)
;; 1
;; racket@> ((two (lambda (n) (+ n 1))) 0)
;; 2
;; racket@> (((add one two) (lambda (n) (+ n 1))) 0)
;; 3
;; racket@> (((add one two) (lambda (n) (+ n 1))) 0)
;; 3

;;; Extended Exercise - Interval Arithmetic
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
		 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
	(p2 (* (lower-bound x) (upper-bound y)))
	(p3 (* (upper-bound x) (lower-bound y)))
	(p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
		   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x (make-interval (/ 1.0 (upper-bound y))
				 (/ 1.0 (lower-bound y)))))

(define (make-interval a b) (cons a b))

;;; Exercise 2.7
(define (lower-bound z)
  (min (car z) (cdr z)))

(define (upper-bound z)
  (max (car z) (cdr z)))

;;; Exercise 2.8 - Subtracting intervals
;;;0	a	b
;;;|----|-------|
;;;|--------|----------|
;;;	    c	       d

;;;0        a          b
;;;|--------|----------|
;;;|----|------------------|
;;;     c                  d

;;;0       a         b  
;;;|-------|---------|
;;;|----|-------| 
;;;     c       d

;;; reforumlating as addition seems a little more elegant.
;;; a - d should always be the lower bound : (lower x) + (- upper y)
;;; b - c should awlays be the upper bound : (upper x) + (- lower y)
(define (sub-interval x y)
  (add-interval x (make-interval (- (upper-bound y)) (- (lower-bound y)))))

;;; Exercise 2.9 - In Noteboook

;;; Exercise 2.10 - Division : Revised to check for division by an interval that spans zero.
;;; I initially interpreted this to mean an interval that has a span of zero, not an interval that spans the
;;; actual number zero. 
;; (define (div-interval x y)
;;   (if (= (- (upper-bound y) (lower-bound y)) 0)
;;       (error "Division by zero-width interval.")
;;       (mul-interval x (make-interval (/ 1.0 (upper-bound y))
;; 				     (/ 1.0 (lower-bound y))))))
(define (div-interval x y)
  (if (<= (* (lower-bound y) (upper-bound y)) 0)
      (error "Division by an interval that spans 0.")
      (mul-interval x (make-interval (/ 1.0 (upper-bound y))
				     (/ 1.0 (lower-bound y))))))

;;; Exercise 2.11
(define (old-mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
	(p2 (* (lower-bound x) (upper-bound y)))
	(p3 (* (upper-bound x) (lower-bound y)))
	(p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
		   (max p1 p2 p3 p4))))

;;; Does this really save anything? Doing way more functions calls here to test the endpoints than
;;; the original multiplications, plus this is not terribly clear.
(define (mul-interval x y)
  (let ((a (lower-bound x))
	(b (upper-bound x))
	(c (lower-bound y))
	(d (upper-bound y)))
    (cond
     ((and (>= c 0) (>= a 0)) (make-interval (* a c) (* b d)))
     ((and (>= c 0) (< b 0)) (make-interval (* a d) (* b c)))
     ((and (< d 0) (>= a 0)) (make-interval (* b c) (* a d)))
     ((and (< d 0) (< b 0)) (make-interval (* b d) (* a c)))
     ((and (>= c 0) (and (< a 0) (>= b 0))) (make-interval (* a d) (* b d)))
     ((and (< d 0) (and (< a 0) (>= b 0))) (make-interval (* c b) (* c a)))
     ((and (and (< c 0) (>= d 0)) (>= a 0)) (make-interval (* b c) (* b d)))
     ((and (and (< c 0) (>= d 0)) (< b 0)) (make-interval (* a d) (* a c)))
     ((and (and (< c 0) (>= d 0)) (and (< a 0) (>= b 0))) (make-interval (min (* a d) (* b c)) (* b d))))))


;;; Making sure new+old provide the same results
(define (equal-interval? x y)
  (and (= (upper-bound x) (upper-bound y))
       (= (lower-bound x) (lower-bound y))))

(define (compare-mul-interval-alg a b c d)
  (let ((x (make-interval a b))
	(y (make-interval c d)))
    (if (equal-interval? (old-mul-interval x y) (mul-interval x y))
	true
	(error "multiplication results differ!" a b c d (old-mul-interval x y) (mul-interval x y)))))

;;; Test Results
;;; 1. All positive endpoints
;; racket@> (compare-mul-interval-alg 1 3 2 4)
;; #t
;;; 1a. All non-negative endpoints
;; racket@> (compare-mul-interval-alg 0 3 2 4)
;; #t
;; racket@> (compare-mul-interval-alg 0 3 0 4)
;; #t
;;; 2. One interval spans zero, the other is positive
;; racket@> (compare-mul-interval-alg -2 3 1 4)
;; #t
;;; 3. Both intervals span zero
;; racket@> (compare-mul-interval-alg -2 3 -1 4)
;; #t
;;; 4. One interval spans zero, one is entirely negative
;; racket@> (compare-mul-interval-alg -2 3 -4 -1)
;; multiplication results differ! -2 3 -4 -1 (-6 . 8) (-12 . 8)
;; Hmmm, what went wrong with this case. Was actually a bug(typo) in old-mul-interval.
;;	     (p3 (* (upper-bound x) (lower-bound x))) should be (lower-bound y)
;; racket@> (compare-mul-interval-alg -2 3 -4 -1)
;; #t
;; That's better.
;;; 5. Both intervals are entirely negative
;; racket@> (compare-mul-interval-alg -3 -2 -4 -1)
;; #t
;;; 6. One interval is positive, one is negative
;; racket@> (compare-mul-interval-alg 2 3 -4 -1)
;; #t
;;; 7. Swap of positive/negative
;; racket@> (compare-mul-interval-alg -3 -2 1 4)
;; #t
;;; 8. Swap of one interval spans zero, one positive
;; racket@> (compare-mul-interval-alg 2 3 -1 4)
;; #t
;;; 8a. One intervals spans zero, the other is non-negative
;; racket@> (compare-mul-interval-alg 0 3 -1 4)
;; #t
;;; 9. Swap of one interval spans zero, one negative
;; racket@> (compare-mul-interval-alg -3 -2 -1 4)
;; #t
;;; 9a. Swap of one interval spans zero, one negative or zero
;; racket@> 
;; #t

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

;; Exercise 2.12
(define (make-center-percent c p)
  (let ((w (* (/ p 100.0) c)))
    (make-center-width c w)))

(define (percent i)
  (* (/ (width i) (center i)) 100.0))
  
;; Exercise 2.13 - Notebook

;;; Exercise 2.14
(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
		(add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
		  (add-interval (div-interval one r1)
				(div-interval one r2)))))

;;; if an interval is done with a very small percentage away from center, then division of an interval into
;;; itself is closer an interval of (1, 1)
;; racket@> (let ((A (make-center-percent 5.0 2)))
;; 	   (div-interval A A))
;; '(0.9607843137254903 . 1.040816326530612)
;; racket@> (let ((A (make-center-percent 5.0 10)))
;; 	   (div-interval A A))
;; '(0.8181818181818182 . 1.222222222222222)
;; racket@> (let ((A (make-center-percent 5.0 25)))
;; 	   (div-interval A A))
;; '(0.6 . 1.6666666666666667)
;; racket@> (let ((A (make-center-percent 5.0 0.01)))
;; 	   (div-interval A A))
;; '(0.9998000199980004 . 1.000200020002)
;; racket@> (let ((A (make-center-percent 5.0 0.00001)))
;; 	   (div-interval A A))
;; '(0.9999998000000202 . 1.0000002000000199)
;; racket@>

;;; It also seems that with interval arithmetic some properties do not hold. For instance, the distributive property :
;; racket@> (let ((a (make-interval 1.0 2.0))
;; 	       (b (make-interval 2.0 3.0))
;; 	       (c (make-interval 3.0 4.0)))
;; 	   (display "a*(b + c) : ") (display (mul-interval a (add-interval b c))) (newline) (display "a*b + b*c : ") (display (add-interval (mul-interval a b) (mul-interval b c))) (newline))
;; a*(b + c) : (5.0 . 14.0)
;; a*b + b*c : (8.0 . 18.0)

;;; Exercise 2.15 - Eva is right, because the error isn't re-introduced if each interval is used only once.

;;; Exercise 2.16 - "Very Difficult" - I'll have to keep thinking about it, but this seems like a very tricky proposition. Equivalent algebraic expressions lead to different answers because what we take to be the rules of algebra do not apply (see the error in the distributive property above) to the interval arithmetic system.



