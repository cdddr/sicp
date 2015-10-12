(define (fib n)
  (cond ((= n 0) 0)
	((= n 1) 1)
	(else (+ (fib (- n 1))
		 (fib (- n 2))))))
(define (fib-linear n)
  (fib-linear-iter 1 0 n))

(define (fib-linear-iter a b count)
  (if (= count 0)
      b
      (fib-linear-iter (+ a b) a (- count 1))))

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
  (when (even? a)
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
;;  Details in Notebook
(define (ex1-19-fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
	((even? count)
	 (fib-iter a
		   b
		   (+ (square q) (square p))
		   (+ (square q) (* 2 q p))
		   (/ count 2)))
	(else (fib-iter (+ (* b q) (* a q) (* a p))
			(+ (* b p) (* a q))
			p
			q
			(- count 1)))))

;; Section 1.2.5 - Greatest Common Divisors
;; Euclid's Algorithm
;; This algorithm is already iterative, and results in O(log n)
;; (define (gcd a b)
;;   (if (= b 0)
;;       a
;;       (gcd b (remainder a b))))

;; Exercise 1.20 - How Many remainder operations are performed
;;  Normal Order Evaluation : Fully Expand then Reduce
;;  (gcd 206 40)
;;    (if (= 40 0) .. (gcd 40 (remainder 206 40)))
;;	(if (= (remainder 206 40) 0) -> (if (= 6 0) ... (gcd (remainder 206 40) (remainder 40 (remainder 206 40))))
;;        (if (= (remainder 40 (remainder 206 40)) 0)) -> (if (= 4 0)
;;          (gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
;;           (if (= (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))
;;           (if (= 2 0) ... )
;;            (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))
;;             (if (= (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))))
;;	       (if (= 0 0) ...)
;;		(remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
;;   In the above, evals only happen in the if statements, until it is fully expanded at the end, so 14+4=18

;;  Applicative Order Evaluation : Evaluate the arguments of the function, then apply
;;  (gcd 206 40)
;;  (gcd 40 (remainder 206 40)) -> (gcd 40 6)
;;  (gcd 6 (remainder 40 6))    -> (gcd 6 4)
;;  (gcd 4 (remainder 6 4))     -> (gcd 4 2)
;;  (gcd 2 (remainder 4 2))     -> (gcd 2 0)
;;  2
;;  In the above, remainder is evaluated 4 times.

;; Section 1.2.6 - Testing For Primality
(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divides? test-divisor n) test-divisor)
	(else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

;; Fermat's Little Theorem - Probalistic Algorithm (O(log n))
(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	 (remainder (square (expmod base (/ exp 2) m))
		    m))
	(else
	 (remainder (* base (expmod base (- exp 1) m))
		    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random-natural (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
	((fermat-test n) (fast-prime? n (- times 1)))
	(else #f)))

;; Exercise 1-21
;; 199, 1999, 7

;; Exercise 1-22
(define (timed-prime-test n)
  (start-prime-test n (current-inexact-milliseconds)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime n (- (current-inexact-milliseconds) start-time))
      #f))

(define (report-prime n elapsed-time)
  (display n)
  (display " *** ")
  (display elapsed-time)
  (newline)
  #t)

;; search consecutive odd numbers for n prime numbers in a range of numbers
;; if first is even, add 1; if last is even subtract 1
(define (search-for-primes first last limit)
  (define (search-iter first last limit)
    (when (and (<= first last) (not (= limit 0))) 
	(search-iter (+ first 2) last (if (timed-prime-test first) (- limit 1) limit))))
  (search-iter (if (even? first) (+ first 1) first)
	       (if (even? last) (- last 1) last)
	       limit))

;; The exercise calls for finding times for 1e3, 1e6, 1e9, but my computer was too fast to differentiate between those.
;; So, I started with 1e9
;;   #;35> (search-for-primes 1000000000 9999999999 3)
;;	100000007 *** 9.0
;;	100000037 *** 9.0					9.33333333
;;	100000039 *** 10.0
;;   #;36> (search-for-primes 10000000000 99999999999 3)		8.49999999999
;;	10000000019 *** 78.0
;;	10000000033 *** 84.0					79.3333333		
;;	10000000061 *** 76.0
;;  #;37> (search-for-primes 100000000000 999999999999 3)		3.28151260504
;;	100000000003 *** 270.0
;;	100000000019 *** 255.0					260.333333
;;	100000000057 *** 256.0
;;  #;38> (search-for-primes 1000000000000 9999999999999 3)		3.06786171575
;;	1000000000039 *** 798.0
;;	1000000000061 *** 800.0					798.666667
;;	1000000000063 *** 798.0
;;  sqrt(10) = 3.16227766017. with the exception of 1e9->1e10, it does indeed grow O(sqrt(n))

;; Exercise 1.23
(define (next n)
  (cond ((= n 2) 3)
	(else (+ n 2))))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divides? test-divisor n) test-divisor)
	(else (find-divisor n (next test-divisor)))))
;; Results :
;; #;44> (search-for-primes 1000000000 9999999999 3)
;; 1000000007 *** 18.0
;; 1000000009 *** 15.0
;; 1000000021 *** 15.0
;; #;45> (search-for-primes 1000000000 9999999999 3)
;; 1000000007 *** 22.0
;; 1000000009 *** 15.0
;; 1000000021 *** 15.0
;; #;46> (search-for-primes 10000000000 99999999999 3)
;; 10000000019 *** 45.0
;; 10000000033 *** 51.0						50.66666 -> ~1.5
;; 10000000061 *** 56.0
;; #;47> (search-for-primes 100000000000 999999999999 3)
;; 100000000003 *** 169.0
;; 100000000019 *** 155.0
;; 100000000057 *** 159.0
;; #;48> (search-for-primes 1000000000000 9999999999999 3)
;; 1000000000039 *** 492.0
;; 1000000000061 *** 487.0
;; 1000000000063 *** 490.0
;; Rough estimates are they are about half, strangely at the lower end it actually increases. Closer look shows they use 1.5 less time.
;; Halved the number of tests, however we did an if test.

;; Exercise 1.24 - Compare fast-prime? to prime?
(define (start-fast-prime-test n start-time)
  (if (fast-prime? n 100)
      (report-fast-prime n (- (current-inexact-milliseconds) start-time))
      #f))
;; All of these were so fast, It looks like I was actually getting random to hang because it was exceeding its limits
;; Using racket scheme I was able to use random-natural to overcome this. Results:
(define (timed-prime-test n)
  (start-prime-test n (current-inexact-milliseconds))
  (start-fast-prime-test n (current-inexact-milliseconds)))

(define (report-prime n elapsed-time)
  (display n)
  (display " *** ")
  (display elapsed-time)
  (display " , ")
  #t)

(define (report-fast-prime n elapsed-time)
  (display elapsed-time)
  (newline)
  #t)

(define (search-for-primes first last limit)
  (define (search-iter first last limit)
    (when (and (<= first last) (not (= limit 0))) 
	(search-iter (+ first 2) last (if (timed-prime-test first) (- limit 1) limit))))
  (search-iter (if (even? first) (+ first 1) first)
	       (if (even? last) (- last 1) last)
	       limit))

;; Had to use really big values to get results consistent enough to draw conclusions from
;; racket@> (search-for-primes 1000000000000 9999999999999 3)
;; 1000000000039 *** 37.510986328125 , 3.452880859375
;; 1000000000061 *** 32.047119140625 , 3.551025390625			ratio of 9.412
;; 1000000000063 *** 32.234130859375 , 3.737060546875
;; racket@> (search-for-primes 10000000000000 99999999999999 3)			2.42
;; 10000000000037 *** 108.427001953125 , 3.706787109375
;; 10000000000051 *** 103.210205078125 , 6.154052734375			ratio of 22.86
;; 10000000000099 *** 100.80908203125 , 3.75
;; racket@> (search-for-primes 100000000000000 999999999999999 3)		3.35
;; 100000000000031 *** 327.134765625 , 4.183837890625
;; 100000000000067 *** 316.9208984375 , 4.22802734375			ratio of 76.65
;; 100000000000097 *** 316.8681640625 , 4.111083984375
;; racket@> (search-for-primes 1000000000000000 9999999999999999 3)		2.89
;; 1000000000000037 *** 1005.551025390625 , 4.432861328125
;; 1000000000000091 *** 999.468017578125 , 4.557861328125			ratio of 221.95
;; 1000000000000159 *** 999.35498046875 , 4.550048828125
;; racket@> (search-for-primes 10000000000000000 999999999999999990 3)		2.58
;; 10000000000000061 *** 3153.963134765625 , 4.804931640625
;; 10000000000000069 *** 3149.189208984375 , 7.012939453125		ratio of 573.74
;; 10000000000000079 *** 3182.421875 , 4.72412109375

;; roughly a constant increase in ratio per multiple of 10, so it does bear out O (logn) growth.


;; Exercise 1.25 - expmod
;; (define (expmod base exp m)
;;   (cond ((= exp 0) 1)
;; 	((even? exp)
;; 	 (remainder (square (expmod base (/ exp 2) m))
;; 		    m))
;; 	(else
;; 	 (remainder (* base (expmod base (- exp 1) m))
;; 		    m))))

;; (define (expmod base exp m)
;;   (remainder (fast-expt base exp) m))

;; (define (fast-expt b n)
;;   (fast-expt-iter b n 1))

;; Here the state variable a when multiplied with b^n becomes an invariant.
;;	For example with n = 3
;;	  (fast-expt-iter 2 3 1) 1*2^3 = 8
;;	  (fast-expt-iter 2 2 2) 2*2^2 = 8
;;	  (fast-expt-iter 4 1 2) 2*4^1 = 8
;;	  (fast-expt-iter 4 0 8) 8*4^0 = 8
;;	So using the state transformation (either multiplying by a by b, or decrementing n by 1
;;	allows the quantity ab^n to be invariant.
;; (define (fast-expt-iter b n a)
;;   (cond ((= n 0) a)
;; 	((even? n) (fast-expt-iter (square b) (/ n 2) a))
;; 	(else (fast-expt-iter b (- n 1) (* a b)))))

;; This actually runs orders of magnitude slower because the results get very large in a hurry. The expmod way never
;; use a number larger than m, based on the footnote in the chapter.

;; Exercise 1.26 - Explicit Multiplication vs Square.
;; Instead of a linear recursion, the rewritten version using an explicit multiplication generates a tree recursion.
;; The tree recursion grows with the depth of the tree O(n).

;; Exercise 1.27 - Testing Carmichael Numbers
;; (define (fermat-test n a)
;;   (= (expmod a n n) a))

;; (define (test-carmichael-number n)
;;   (define (tcn-iter a n fooled?)
;;     (cond ((= a n) #t)
;; 	  ((not fooled?) fooled?)
;; 	  (else
;; 	   (tcn-iter (+ a 1) n (fermat-test n a)))))
;;   (tcn-iter 0 n #t))

;; racket@> (test-carmichael-number 2821)
;; #t
;; racket@> (test-carmichael-number 561)
;; #t
;; racket@> (test-carmichael-number 619)
;; #t
;; racket@> (test-carmichael-number 620)
;; #f
;; racket@> (test-carmichael-number 557)
;; #t
;; racket@> (test-carmichael-number 561)
;; #t
;; racket@> (test-carmichael-number 1105)
;; #t
;; racket@> (test-carmichael-number 1729)
;; #t
;; racket@> (test-carmichael-number 2465)
;; #t
;; racket@> (test-carmichael-number 2821)
;; #t
;; racket@> (test-carmichael-number 6601)
;; #t

;; Exercise 1.28 - Miller Rabin Primality Test
(define (non-trivial-sqrt? n m)
  (cond ((= n 1) #f)
	((= n (- m 1)) #f)
	(else (= (remainder (square n) m) 1))))

(define (check-non-trivial-sqrt? n m)
  (if (non-trivial-sqrt? n m)
      0
      (remainder (square n) m)))

(define (expmod-rabin base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	 (check-non-trivial-sqrt? (expmod-rabin base (/ exp 2) m) m))
	(else
	 (remainder (* base (expmod-rabin base (- exp 1) m))
		    m))))

(define (miller-rabin n)
  (miller-rabin-test (- n 1) n))

(define (miller-rabin-test a n)
  (cond ((= a 0) #t)
	((= (expmod-rabin a (- n 1) n) 1) (miller-rabin-test (- a 1) n))
	(else #f)))

;; racket@> (search-for-primes 500 1000 10)
;; 503 *** 0.002197265625 , 0.243896484375
;; 509 *** 0.001953125 , 0.236083984375
;; 521 *** 0.0009765625 , 0.219970703125
;; 523 *** 0.0009765625 , 0.22900390625
;; 541 *** 0.0009765625 , 0.238037109375
;; 547 *** 0.001953125 , 0.22705078125
;; 557 *** 0.001953125 , 0.23388671875
;; 0.230224609375
;; 563 *** 0.002197265625 , 0.29296875
;; 569 *** 0.0009765625 , 0.238037109375
;; racket@> (miller-rabin 503)
;; #t
;; racket@> (miller-rabin 509)
;; #t
;; racket@> (miller-rabin 521)
;; #t
;; racket@> (miller-rabin 523)
;; #t
;; racket@> (miller-rabin 541)
;; #t
;; racket@> (miller-rabin 547)
;; #t
;; racket@> (miller-rabin 557)
;; #t
;; racket@> (miller-rabin 561)
;; #f
;; racket@> (miller-rabin 563)
;; #t
