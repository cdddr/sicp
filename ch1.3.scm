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
