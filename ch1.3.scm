;; Section 1.3
(define (cube x)
  (* x x x))

;; Section 1.3.1
;; (define (sum-integers a b)
;;   (if (> a b)
;;       0
;;       (+ a (sum-integers (+ a 1) b))))

;; (define (sum-cubes a b)
;;   (if (> a b)
;;       0
;;       (+ (cube a) (sum-cubes (+ a 1) b))))

;; (define (pi-sum a b)
;;   (if (> a b)
;;       0
;;       (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))

;; abstraction
;; (define (<name> a b)
;;   (if (> a b)
;;       0
;;       (+ (<term> a)
;; 	 (<name> (<next> a) b))))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
	 (sum term (next a) next b))))

(define (inc n) (+ n 1))

(define (sum-cubes a b)
  (sum cube a inc b))

(define (identity n) n)

(define (sum-integers a b)
  (sum identity a inc b))

(define (pi-sum a b)
  (define (pi-term x)
    (+ (/ 1.0 (* x (+ x 2)))))
  (define (pi-next x) (+ x 4))
  (sum pi-term a pi-next b))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (*  (sum f (+ a (/ dx 2)) add-dx b) dx))

;; FIXME : keeping a counter that is set! in the next term is kind of gross.
;; FIXME BAD : these two versions are not the same. I think floating point error is causing it to drift away.
;;             they should be functionally the same however.
(define (simpsons-rule-bad f a b n)
  (define h (/ (- b a) n))
  (define k 0)
  (define (simpsons-term x)
    (* (cond ((or (= k 0) (= k n)) 1)
	     ((even? k) 2)
	     (else 4))
       (f x)))
  (define (simpsons-next x)
    (set! k (+ k 1))
    (+ x h))
  (* (sum simpsons-term a simpsons-next b) (/ h 3.0)))

;; This version is very accurate (and better looking).
;;   n	    integral		|  simpsons
;;  100 |  0.24998750000000042	|  0.24999999999999992	|
;; 1000 |  0.249999875000001	|  0.2500000000000003	|

(define (simpsons-rule f a b n)
  (define h (/ (- b a) n))
  ;;don't need to sum from a->b in this case, we can sum from 0 n and calculate a.
  (define (simpsons-term k)
    (* (cond ((or (= k 0) (= k n)) 1)
	     ((even? k) 2)
	     (else 4))
       (f (+ a (* k h)))))
  (* (sum simpsons-term 0 inc n) (/ h 3.0)))

;; Exercise 1.30 - Iterative Summation
;; Check on results : racket@> (* 8 (pi-sum 1 10000000))
;;		      3.14159245358981
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (+ (term a) result))))
  (iter a 0))

;; Exercise 1.31 - Abstract Product Procedure
(define (product term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b))))

;; Iterative version
;; Four time speedup
;; racket@> (time (pi-product 1 10000))
;; cpu time: 4177 real time: 4173 gc time: 92
;; 0.7854177966336159
;; racket@> (time (pi-product 1 10000))
;; cpu time: 1030 real time: 1029 gc time: 514
;; 0.7854177966336159
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (* (term a) result))))
  (iter a 1))

(define (dec x) (- x 1))

(define (square x) (* x x))

(define (factorial n)
  (if (= n 0)
      1
      (product identity 1 inc n)))

(define (pi-product a b)
  (define (pi-term a)
    (define b (* 2 a))
    (/ (* b (+ b 2)) (square (+ (* 2 a) 1))))
  (exact->inexact (product pi-term a inc b)))

;; Exercise 1.32 - Abstracting Further To Accumulate
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate combiner null-value term (next a) next b))))


;; Results
;; Recursive
;; racket@> (time (* 8 (pi-sum 1 10000000)))
;; cpu time: 2083 real time: 2088 gc time: 1134
;; 3.141592453589793
;; racket@> (time (pi-product 1 10000))
;; cpu time: 4667 real time: 4668 gc time: 525
;; 0.7854177966336159
;; Iterative
;; racket@> (time (* 8 (pi-sum 1 10000000)))
;; cpu time: 380 real time: 381 gc time: 8   <-----+
;; 3.14159245358981				   |
;; racket@> (time (pi-product 1 10000))            | Nice!
;; cpu time: 520 real time: 521 gc time: 12  <-----+ 
;; 0.7854177966336159

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))

;; Iterative Version
(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (combiner (term a) result))))
  (iter a null-value))

;; Exercise 1.33 - Filter Accumulate
(define (filtered-accumulate predicate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (combiner (if (predicate a)
				     (term a)
				     null-value)
				 result))))
  (iter a null-value))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divides? test-divisor n) test-divisor)
	(else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (if (= n 1)
      #f
      (= n (smallest-divisor n))))

(define (sum-prime-squares a b)
  (filtered-accumulate prime? + 0 square a inc b))

(define (product-relatively-prime b)
  (define (relatively-prime? i)
    (= (gcd i b) 1))
  (filtered-accumulate relatively-prime? * 1 identity 1 inc b))

;; Section 1.3.3
(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
	midpoint
	(let ((test-value (f midpoint)))
	  (cond ((positive? test-value)
		 (search f neg-point midpoint))
		((negative? test-value)
		 (search f midpoint pos-point))
		(else midpoint))))))

(define (average a b)
  (/ (+ a b) 2))

(define (close-enough? x y)
  (< (abs (- x y)) 0.001))

(define (half-interval-method f a b)
  (let ((a-value (f a))
	(b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
	   (search f a b))
	  ((and (negative? b-value) (positive? a-value))
	   (search f b a))
	  (else
	   (error "Values are not of opposite sign" a b)))))

;; racket@> (half-interval-method (lambda (x) (- (* x x x) (* 2 x) 3))
;; 			       1.0 2.0)
;; 1.89306640625

;;; Fixed point function
;;;   A function where f(x) = x. Approximated by applying f(x), f(f(x)), f(f(f(x))) until the value does not change much
(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))

;; racket@> (fixed-point cos 1.0)
;; 0.7390822985224024
;; racket@> (fixed-point (lambda (y) (+ (sin y) (cos y)))
;; 		      1.0)
;; 1.2587315962971173

;; This process is similar to how square roots were computed in section 1.1.7. Using a fixed point approach for square roots requires reformulating a bit. y^{2}=x -> y = x / y

(define (sqrt x)			;; does not converge. just cycles back and forth.
  (fixed-point (lambda (y) (/ x y))
	       1.0))

(define (sqrt x)			;; this does converge, due to average damping. equivalent to 1.1.7
  (fixed-point (lambda (y) (average y (/ x y)))
	       1.0))


;; Exercise 1.35 - Show that the golden ratio is a fixed point of the transformation x |-> 1 + 1/x
;; \phi^{2} = \phi + 1 , dividing by \phi => \phi = 1 + 1/\phi, thus it is a fixed poinnt of x |-> 1 + 1/x.
(define (golden-ratio)
  (fixed-point (lambda (x) (+ 1 (/ 1 x)))
	       1.0))

;; Exercise 1.36 - x^{x} = 1000
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))

;; 34 steps
;; 2.0, 9.965784284662087, 3.004472209841214, 6.279195757507157, 3.759850702401539, 5.215843784925895, 4.182207192401397, 4.8277650983445906, 4.387593384662677, 4.671250085763899, 4.481403616895052, 4.6053657460929, 4.5230849678718865, 4.577114682047341, 4.541382480151454, 4.564903245230833, 4.549372679303342, 4.559606491913287, 4.552853875788271, 4.557305529748263, 4.554369064436181, 4.556305311532999, 4.555028263573554, 4.555870396702851, 4.555315001192079, 4.5556812635433275, 4.555439715736846, 4.555599009998291, 4.555493957531389, 4.555563237292884, 4.555517548417651, 4.555547679306398, 4.555527808516254, 4.555540912917957, 4.555532270803653
(define (ex1-36)
  (fixed-point (lambda (x) (/ (log 1000) (log x)))
	       25.0))

;; 9 steps
;; 2.0, 5.9828921423310435, 4.922168721308343, 4.628224318195455, 4.568346513136242, 4.5577305909237005, 4.555909809045131, 4.555599411610624, 4.5555465521473675, 4.555537551999825
(define (ex1-36-damping)
  (fixed-point (lambda (x) (average x (/ (log 1000) (log x))))
	       25.0))

;; Exercise 1.37 - k-term finite continued fraction
