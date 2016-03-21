;;; Symbolic Data

(define (memq item x)
  (cond ((null? x) false)
	((eq? item (car x)) x)
	(else (memq item (cdr x)))))

;;; Exercise 2.53
'(a b c)
'((george))
'((y1 y2))
'(y1 y2)
false
false
'(red shoes blue socks)

;; racket@> (list 'a 'b 'c)
;; '(a b c)
;; racket@> (list (list 'george))
;; '((george))
;; racket@> (cdr '((x1 x2) (y1 y2)))
;; '((y1 y2))
;; racket@> (cadr '((x1 x2) (y1 y2)))
;; '(y1 y2)
;; racket@> (pair? (car '(a short list)))
;; #f
;; racket@> (memq 'red '((red shoes (blue socks))))
;; #f
;; racket@> (memq 'red '(red shoes blue socks))
;; '(red shoes blue socks)
;; racket@>

(define (equal? l1 l2)
  (cond ((and (not (pair? l1)) (not (pair? l2)))
	 (eq? l1 l2))
	((and (pair? l1) (pair? l2))
	 (and (equal? (car l1) (car l2))
	      (equal? (cdr l1) (cdr l2))))
	(else
	 #f)))
;; racket@> (equal? '(a b c d) '(a b c))
;; #f
;; racket@> (equal? '(a b c d) '(a b c d))
;; #t
;; racket@> (equal? '(a b c d) '(a b c d))
;; #t
;; racket@> (equal? '(a b c d) '(a b c))
;; #f
;; racket@> (equal? '(a b c d) '(a b c d))
;; #t
;; racket@> (equal? '(a b c d) '(a b (c d)))
;; #f
;; racket@> (equal? '(a b (c d)) '(a b (c d)))
;; #t

;; Exercise 2.55
;; Under the hood, ' must use the quote function. So when the car of a quoted quote is taken,
;; it prints out the first member of the function call, the function

(define (deriv exp var)
  (cond ((number? exp) 0)
	((variable? exp)
	 (if (same-variable? exp var) 1 0))
	((sum? exp)
	 (make-sum (deriv (addend exp) var)
		   (deriv (augend exp) var)))
	((product? exp)
	 (make-sum
	  (make-product (multiplier exp)
			(deriv (multiplicand exp) var))
	  (make-product (deriv (multiplier exp) var)
			(multiplicand exp))))
	(else
	 (error "unknown expression type -- DERIVE" exp))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
	((=number? a2 0) a1)
	((and (number? a1) (number? a2) (+ a1 a2)))
	(else (list '+ a1 a2))))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
	((=number? m1 1) m2)
	((=number? m2 1) m1)
	((and (number? m1) (number? m2)) (* m1 m2))
	(else (list '* m1 m2))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))
(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))


;; racket@> (deriv '(* x y) 'x)
;; '(+ (* x 0) (* 1 y))
;; racket@> (deriv '(* x y) 'x)
;; '(+ (* x 0) (* 1 y))
;; racket@> (deriv '(* (* x y) (+ x 3)) 'x)
;; '(+ (* (* x y) 1) (* (+ (* x 0) (* 1 y)) (+ x 3)))
;; racket@> (deriv '(* x y) 'x)
;; 'y
;; racket@> (deriv '(* (* x y) (+ x 3)) 'x)
;; '(+ (* x y) (* y (+ x 3)))
;; racket@> (deriv '(+ x 3) 'x)
;; 1

