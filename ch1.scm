(define (square x)
  (* x x))
(define (sum-squares x y)
  (+ (square x) (square y)))
(define (ex1-3 a b c)
  (if (> a b)
      (if (> b c)
	  (sum-squares a b)
	  (sum-squares a c))
      (if (> c b)
	  (sum-squares b c)
	  (if (> c a)
	      (sum-squares b c)
	      (sum-squares b a)))))

;;1.1.7
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
		 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt-iter2 guess x)
  (new-if (good-enough? guess x)
	  guess
	  (sqrt-iter2 (improve guess x) x)))

(define (new-if predicate then-clause else-clause)
  (display "entered new-if!")
  (newline)
  (cond (predicate then-clause)
	(else else-clause)))

;; 1.2.2 - Example - Counting Change
(define count-change-calls 0)
(define (count-change amount)
  (set! count-change-calls 0)
  (print (cc amount 5))
  (print "steps : " count-change-calls))

(define (cc amount kinds-of-coins)
  (set! count-change-calls (+ count-change-calls 1))
  (cond ((= amount 0) 1)
	((or (< amount 0) (= kinds-of-coins 0)) 0)
	(else (+ (cc amount
		     (- kinds-of-coins 1))
		 (cc (- amount
			(first-denomination kinds-of-coins))
		     kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
	((= kinds-of-coins 2) 5)
	((= kinds-of-coins 3) 10)
	((= kinds-of-coins 4) 25)
	((= kinds-of-coins 5) 50)))


;; Exercise 1.11
;;  -Recursive
(define (ex1-11-f n)
  (cond ((< n 3) n)
	(else (+ (ex1-11-f (- n 1))
		 (* 2 (ex1-11-f (- n 2)))
		 (* 3 (ex1-11-f (- n 3)))))))

;;  -Iterative
(define (ex1-11-f-iter n)
  (ex1-11-iter 2 1 0 3 n))

(define (ex1-11-iter f1 f2 f3 counter n)
  (cond ((< n 3) n)
	((> counter n) f1)
	(else
	 (ex1-11-iter (+ f1
			 (* 2 f2)
			 (* 3 f3)) f1 f2 (+ counter 1) n))))

;; Exercise 1.12 - Pascal's Triangle
;; 1
;; 1 1
;; 1 2 1
;; 1 3 3 1
;; 1 4 6 4 1
;; 1 5 10 10 5 1
;; column, depth
;; Writing an iterative function seems like a lot of work compared to writing a simple for loop when all I want to do is loop.
;; Perhaps this will get better over time, but I imagine writing some sort of macro is a better way.
(define (ex1-12-pascal col depth)
  (cond ((= col 1) 1)
	((= col depth) 1)
	(else (+ (ex1-12-pascal col (- depth 1))
		 (ex1-12-pascal (- col 1) (- depth 1))))))

(define (ex1-12-pascal-print depth)
  (ex1-12-pascal-print-iter 1 depth))

(define (ex1-12-pascal-print-iter i n)
  (cond ((= i n) (ex1-12-pascal-print-row i))
	(else
	 (ex1-12-pascal-print-row i)
	 (ex1-12-pascal-print-iter (+ i 1) n))))

(define (ex1-12-pascal-print-row row)
  (ex1-12-pascal-print-row-iter 1 row))

(define (ex1-12-pascal-print-row-iter i n)
  (cond ((= i n) (display (string-append (number->string (ex1-12-pascal i n)) "\n")))
	(else
	 (display (string-append (number->string (ex1-12-pascal i n)) " "))
	 (ex1-12-pascal-print-row-iter (+ i 1) n))))

;; Exercise 1.13-1.14 - See SICP Notebook

;; Exercise 1.15
(define (ex1-15-cube x)
  (* x x x))

(define (ex1-15-p x)
  (set! ex1-15-p-calls (+ ex1-15-p-calls 1))
  (- (* 3 x) (* 4 (ex1-15-cube x))))

(define (ex1-15-sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (ex1-15-p (ex1-15-sine (/ angle 3.0)))))

(define ex1-15-p-calls 0)
(define (ex1-15-sin angle)
  (set! ex1-15-p-calls 0)
  (ex1-15-sine angle)
  (print "ex1-15 calls p for angle " angle " : " ex1-15-p-calls))

;; Section 1.2.4 -  Exponentiation
(define (expt-recur b n)
  (if (= n 0)
      1
      (* b (expt-recur b (- n 1)))))

;; Exponentiation - Recursion - Estimating growth of space/time:
;; For each call, the recursive function will itself n times. So number of steps wise, it grows O(n).
;; As far as space goes, the substitution looks like this
;; (* b (expt-recur b (- n 1)))
;; (* b (* b (expt-recur b (- n 2)))
;; . . . so as n grows we need n "space". So it grows in space/time on O(n). Thus it is a linear recursive process.

(define (expt-iter b n)
  (expt-iter-iter b n 1))

(define (expt-iter-iter b counter product)
  (if (= counter 0)
      product
      (expt-iter-iter b
		      (- counter 1)
		      (* b product))))

;; Exponentiation - Iteration - Estimating growth of space/time:
;; This version will still grow on O(n) in terms of steps, we still need to multiply n times to do an exponentiation.
;; However, in terms of space here is how the substitution breaks down:
;;	(expt-iter-iter b (- counter 1) (* b product))
;;	(expt-iter-iter b (- (- counter 1) 1) (* b (* b product)))
;; Thus at each, step, we only need to account for the same amount of space. So it grows O(1).


;; Successive Squares - Faster exponentiation
;; How does the below grow in space/time?
;;	Time  - It will take roughly log2(n) steps for each n. So it is O(log n)
;;	Space - For n steps, we will need to keep track of log2 n calls to fast-expt, so it is O(log n).
;; (define (even? n)
;;   (= (remainder n 2) 0))

(define (fast-expt b n)
  (cond ((= n 0) 1)
	((even? n) (square (fast-expt b (/ n 2))))
	(else (* b (fast-expt b (- n 1))))))

;; Exercise 1.16 - Iterative Exponentiation - State Variables & Invariants
(define (ex1-16 b n)
  (fast-expt-iter b n 1))

;; Here the state variable a when multiplied with b^n becomes an invariant.
;;	For example with n = 3
;;	  (fast-expt-iter 2 3 1) 1*2^3 = 8
;;	  (fast-expt-iter 2 2 2) 2*2^2 = 8
;;	  (fast-expt-iter 4 1 2) 2*4^1 = 8
;;	  (fast-expt-iter 4 0 8) 8*4^0 = 8
;;	So using the state transformation (either multiplying by a by b, or decrementing n by 1
;;	allows the quantity ab^n to be invariant.
(define (fast-expt-iter b n a)
  (cond ((= n 0) a)
	((even? n) (fast-expt-iter (square b) (/ n 2) a))
	(else (fast-expt-iter b (- n 1) (* a b)))))


;; Exercise 1.17 - Multiplication defined as addition
;; For each increase in n, we will have to do a linear amount of steps, O(n)
(define ex1-17-n-count 0)
(define ex1-17-log-count 0)
(define (ex1-17-count a b)
  (set! ex1-17-n-count 0)
  (set! ex1-17-log-count 0)
  (print "result for linear algorithm : " (ex1-17-* a b))
  (print "calls for linear algorithm : " ex1-17-n-count)
  (print "result for log algorithm : " (ex1-17-*-log a b))
  (print "calls for log algorithm : " ex1-17-log-count))

(define (ex1-17-* a b)
  (set! ex1-17-n-count (+ ex1-17-n-count 1))
  (if (= b 0)
      0
      (+ a (ex1-17-* a (- b 1)))))

(define (ex1-17-*-log a b)
  (set! ex1-17-log-count (+ ex1-17-log-count 1))
  (cond ((= b 0) 0)
	((even? b) (ex1-17-double (ex1-17-*-log a (ex1-17-halve b))))
	(else
	 (+ a (ex1-17-*-log a (- b 1))))))

(define (ex1-17-double a)
  (+ a a))

(define (ex1-17-halve a)
  (if (even? a)
      (/ a 2)))

;; Exercise 1.18 - Iterative Multiplication in terms of addition (Russian Peasant Method)
;;   Similarl to Ex 1.16, A state variable will be introduced and the three arguments will
;;   form an invariant.
;;   State variable : s, Invariant : ab + s
;;	Example a=1, b = 4, s=0:
;;	  (ex1-18-*-iter 1 4 0) 1*4+0=4
;;	  (ex1-18-*-iter 2 2 0) 2*2+0=4
;;	  (ex1-18-*-iter 4 1 0) 4*1+0=4
;;	  (ex1-18-*-iter 4 0 4) 4*0+4=4
(define (ex1-18-* a b)
  (ex1-18-*-iter a b 0))

(define (ex1-18-*-iter a b s)
  (cond ((= b 0) s)
	((even? b) (ex1-18-*-iter (ex1-17-double a) (ex1-17-halve b) s))
	(else
	 (ex1-18-*-iter a (- b 1) (+ s a)))))
;; Exercise 1.19 - Fibonacci in logarithm
