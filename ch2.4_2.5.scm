;;; Section 2.4 - Multiple Representations for Abstract Data

;;; Helpers
(define (square x)
  (* x x))

;;; Complex Numbers represented in rectangular and polar form

;;; Rectangular Representation
(define (real-part z) (car z))

(define (imag-part z) (cdr z))

(define (magnitude z)
  (sqrt (+ (square (real-part z)) (square (imag-part z)))))

(define (angle z)
  (atan (imag-part z) (real-part z)))

(define (make-from-real-imag r i) (cons r i))

(define (make-fram-mag-ang r a)
  (cons (* r (cos a)) (* r (sin a))))

;;; Polar Representation
(define (real-part z)
  (* (magnitude z) (cos (angle z))))

(define (imag-part z)
  (* (magnitude z) (sin (angle z))))

(define (magnitude z)
  (car z))

(define (angle c)
  (cdr z))

(define (make-from-real-imag r i)
  (cons (sqrt (+ (square r) (square i)))
	(atan i r)))

(define (make-fram-mag-ang r a)
  (cons r a))

;;; The above procedures should have the property that for any complex number z
;;; (make-from-real-imag (real-part z) (imag-part z))
;;; (make-from-mag-ang (magnitude z) (angle z))
;;; Will return the same complex number z.

(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
		       (+ (imag-part z1) (imag-part z2))))

(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
		       (- (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
		     (+ (angle z1) (angle z2))))

(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
		     (- (angle z1) (angle z2))))


;;; Section 2.4.2 - Tagged Data
(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))

(define (polar? z)
  (eq? (type-tag z) 'polar))

;;; Rectangular Representation
(define (real-part-rectangular z) (car z))

(define (imag-part-rectangular z) (cdr z))

(define (magnitude-rectangular z)
  (sqrt (+ (square (real-part-rectangular z)) (square (imag-part-rectangular z)))))

(define (angle-rectangular z)
  (atan (imag-part-rectangular z) (real-part-rectangular z)))

(define (make-from-real-imag-rectangular r i)
  (attach-tag 'rectangular (cons r i)))

(define (make-fram-mag-ang-rectangular r a)
  (attach-tag 'rectangular (cons (* r (cos a)) (* r (sin a)))))

;;; Polar Representation
(define (real-part-polar z)
  (* (magnitude-polar z) (cos (angle-polar z))))

(define (imag-part-polar z)
  (* (magnitude-polar z) (sin (angle-polar z))))

(define (magnitude-polar z)
  (car z))

(define (angle-polar c)
  (cdr z))

(define (make-from-real-imag-polar r i)
  (attach-tag 'polar (cons (sqrt (+ (square r) (square i)))
			   (atan i r))))

(define (make-fram-mag-ang-polar r a)
  (attach-tag 'polar (cons r a)))

(define (real-part z)
  (cond ((rectangular? z)
	 (real-part-rectangular (contents z)))
	((polar? z)
	 (real-part-polar (contents z)))
	(else
	 (error "Unknown type -- REAL-PART" z))))

(define (imag-part z)
  (cond ((rectangular? z)
	 (imag-part-rectangular (contents z)))
	((polar? z)
	 (imag-part-polar (contents z)))
	(else
	 (error "Unknown type -- IMAG-PART" z))))

(define (magnitude z)
  (cond ((rectangular? z)
	 (magnitude-rectangular (contents z)))
	((polar? z)
	 (magnitude-polar (contents z)))
	(else
	 (error "Unknown type -- MAGNITUDE" z))))

(define (angle z)
  (cond ((rectangular? z)
	 (angle-rectangular (contents z)))
	((polar? z)
	 (angle-polar (contents z)))
	(else
	 (error "Unknown type -- ANGLE" z))))

(define (make-from-real-imag x y)
  (make-from-real-imag-rectangular x y))

(define (make-from-mag-ang r a)
  (make-from-mag-ang-polar r a))
  
  
;;; Execise 2.73
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

(define (addend s) (car s))
(define (augend s) (cadr s))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (car p))
(define (multiplicand p) (cadr p))
  		  							   
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define op-table (make-hash))
(define (put op type item)
  (hash-set! op-table (list op type) item))
(define (get op type)
  (if (hash-has-key? op-table (list op type))
      (hash-ref op-table (list op type))
      #f))
(define (deriv exp var)
  (cond ((number? exp) 0)
	((variable? exp) (if (same-variable? exp var) 1 0))
	(else 
	 ((get 'deriv (operator exp)) (operands exp)
	  var))))

;; b)
(define (deriv-product exp var)
  (make-sum
   (make-product (multiplier exp)
		 (deriv (multiplicand exp) var))
   (make-product (deriv (multiplier exp) var)
		 (multiplicand exp))))
					  
(define (deriv-sum exp var)
	(make-sum 
		(deriv (addend exp) var)
		(deriv (augend exp) var)))					  
(put 'deriv '+ deriv-sum)
(put 'deriv '* deriv-product)

;; racket@> (deriv '(* x y) 'x)
;; 'y
;; racket@> (deriv '(* (* x y) (+ x 3)) 'x)
;; '(+ (* x y) (* y (+ x 3)))
;; racket@> (deriv '(+ x 3) 'x)
;; 1

;; c)
(define (make-exponentiation base exponent)
  (cond ((= exponent 0) 1)
	((= exponent 1) base)
	(else (list '** base exponent))))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base x)
  (car x))
(define (exponent x)
  (cadr x))
  
(define (deriv-exponent exp var)
	(make-product
		(make-product (exponent exp)
			(make-exponentiation (base exp) (- (exponent exp) 1)))
		(deriv (base exp) var)))
		
(put 'deriv '** deriv-exponent)

;; racket@> (deriv '(** x 2) 'x)
;; '(* 2 x)
;; racket@> (deriv '(** x 4) 'x)
;; '(* 4 (** x 3))

;; d)
;; All we should need is....
;; (put '+ 'deriv deriv-sum)
;; (put '+ 'deriv deriv-product)
;; (put '+ 'deriv deriv-exponent)


;;; Exercise 2.75
(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part)
	   (* r (cos a)))
	  ((eq? op 'imag-part)
	   (* r (sin a)))
	  ((eq? op 'magnitude) r)
	  ((eq? op 'angle) a)
	  (else
	   (error "Unknown op -- MAKE-FROM-MAG-ANG" op))))
  dispatch)

;; racket@> ((make-from-mag-ang 5 (asin (/ 4 5))) 'imag-part)
;; 4.0
;; racket@> ((make-from-mag-ang 5 (asin (/ 4 5))) 'real-part)
;; 3.0
;; racket@> ((make-from-mag-ang 5 (asin (/ 4 5))) 'magnitude)
;; 5
;; racket@> ((make-from-mag-ang 5 (asin (/ 4 5))) 'angle)
;; 0.9272952180016123

;;; Exercise 2.76
;;; I think for a system with lots of types, a data-directed approach would be easier to maintain and for a system
;;; with few types, but a large amount of operations, a message passing approach would be easier.

;; ;; Section 2.5 - Systems with Generic Operations
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	  (apply proc (map contents args))
	  (error
	   "No method found for these types -- APPLY-GENERIC"
	   (list op type-tags))))))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
	
(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
		 (* (numer y) (denom x)))
	      (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
		 (* (numer y) (denom x)))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
	      (* (denom x) (denom y))))

  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
	      (* (denom x) (numer y))))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (install-rectangular-package)
  (define (real-part z)  (car z))
  (define (imag-part z)  (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
	     (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))

  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y)
	 (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a)
	 (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
	  (atan y x)))

  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y)
	 (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a)
	 (tag (make-from-mag-ang r a))))
  'done)

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'real-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))
(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))

  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
			 (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
			 (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
		       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
		       (- (angle z1) (angle z2))))

  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

;; racket@> (add (make-rational 1 2) (make-rational 1 2))
;; (rational rational)
;; '(rational 1 . 1)

;; racket@> (add (make-complex-from-real-imag 3 4) (make-complex-from-mag-ang 1 0))
;; '(complex rectangular 4 . 4)

;;; Does not work with mixed types yet.
;; racket@> (add (make-complex-from-real-imag 3 4) (make-scheme-number 1))
;; hash-ref: no value found for key
;;   key: '(add (complex scheme-number))

;; ;; Exercises 2.77 - 2.80 - TODO
;; ;; I'm going to come back to these once I get a working get/put system going.
;; ;;; Hashtables maybe?
;; ;;; (put <op> <type> <item>)
;; ;;; (get <op> <type>)

;;; Exercise 2.77
;; This works because we have already defined real-part, imag-part, magnitude, and angle in terms of rectangular/polar
;; cooridnates and apply-generic. All we need to do is add these complex type in the op-table.
;; In this case, apply generic should be called twice. Once for the complex type and again for the magnitude. The
;; first call to apply generic dispatches to the top level generic but passes in 'rectangular as the type. The
;; second apply-generic dispatches to the private magnitude function inside the rectangular package.

;; racket@> (magnitude (make-complex-from-real-imag 3 4))
;; apply-generic - (complex) : #<procedure:magnitude>
;; apply-generic - (rectangular) : #<procedure:magnitude>
;; 5

;;; Exercise 2.78
;;; I'm not 100% sure this is what they were after in the problem, but it works. I just represent the lack of type
;;; with the empty list.

(define (attach-tag type-tag contents)
  (if (number? contents)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (cond ((number? datum) 'scheme-number)
	    ((symbol? datum)
	     (error "Bad tagged datum -- TYPE-TAG" datum)))))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (cond ((number? datum) datum)
	    ((symbol? datum)
	     (error "Bad tagged datum -- CONTENTS" datum)))))


(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number) +)
  (put 'sub '(scheme-number scheme-number) -)
  (put 'mul '(scheme-number scheme-number) *)
  (put 'div '(scheme-number scheme-number) /)  
  'done)

;;; Exercise 2.79
(define (equ? x y) (apply-generic 'equ? x y))

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number) +)
  (put 'sub '(scheme-number scheme-number) -)
  (put 'mul '(scheme-number scheme-number) *)
  (put 'div '(scheme-number scheme-number) /)
  (put 'equ? '(scheme-number scheme-number) =)		;just uses the built-in = for numbers
  (put '=zero? '(scheme-number) (lambda (x) (= x 0)))
  'done)

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
		 (* (numer y) (denom x)))
	      (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
		 (* (numer y) (denom x)))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
	      (* (denom x) (denom y))))

  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
	      (* (denom x) (numer y))))
  (define (equ? x y)
    (and (= (numer x) (numer y))
	 (= (denom x) (denom y))))
  (define (=zero? x)
    (= (numer x) 0))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'equ? '(rational rational) equ?)
  (put '=zero? '(rational) =zero?)
  'done)

(define (install-rectangular-package)
  (define (real-part z)  (car z))
  (define (imag-part z)  (display z) (newline) (display (cdr z)) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
	     (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  (define (equ? z1 z2)
    (and (= (real-part z1) (real-part z2))
	 (= (imag-part z1) (imag-part z2))))

  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y)
	 (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a)
	 (tag (make-from-mag-ang r a))))
  (put 'equ? '(rectangular rectangular) equ?)
  'done)

(define (install-polar-package)
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
	  (atan y x)))
  (define (equ? z1 z2)
    (and (= (magnitude z1) (magnitude z2))
	 (= (angle z1) (angle z2))))

  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y)
	 (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a)
	 (tag (make-from-mag-ang r a))))
  (put 'equ? '(polar polar) equ?)
  'done)


(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))

  (define (add-complex z1 z2)
    (display z1) (display " , ") (display z2) (newline)
    (display (real-part z1)) (display " , ") (display (real-part z2)) (display " : ")
    (display (imag-part z1)) (display " , ") (display (imag-part z2)) (newline)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
			 (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
			 (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
		       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
		       (- (angle z1) (angle z2))))
  (define (equ? z1 z2)
    (and (= (real-part z1) (real-part z2))
	 (= (imag-part z1) (imag-part z2))))
  (define (=zero? z)
    (equ? z (make-complex-from-real-imag 0 0)))

  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  (put 'equ? '(complex complex) equ?)   ;added change for this exercise.
  (put '=zero? '(complex) =zero?)	;added for exercise 2.80.
  'done)

;; racket@> (equ? (make-complex-from-real-imag 3 4) (make-complex-from-mag-ang 5 (asin (/ 4 5))))
;; #t
;; racket@> (equ? (make-rational 1 2) (make-rational 1 3))
;; #f
;; racket@> (equ? (make-rational 1 2) (make-rational 1 2))
;; #t
;; racket@> (equ? (make-rational 1 2) (make-rational 2 4))
;; #t
;; racket@> (equ? 1 1)
;; #t
;; racket@> (equ? 1 1.0)
;; #t
;; racket@> (equ? 1 2.0)
;; #f

;;; Exercise 2.80 =zero?

(define (=zero? x) (apply-generic '=zero? x))

;; racket@> (=zero? 1)
;; #f
;; racket@> (=zero? 0)
;; #t
;; racket@> (=zero? (make-complex-from-mag-ang 0 0))
;; #t
;; racket@> (=zero? (make-complex-from-real-imag 1 0))
;; #f
;; racket@> (=zero? (make-complex-from-real-imag 0 0))
;; #t
;; racket@> (=zero? (make-rational 0 1))
;; #t

;;; Coercion

(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))

(define coercion-table (make-hash))
(define (put-coercion type1 type2 proc)
  (hash-set! coercion-table (list type1 type2) proc))
(define (get-coercion type1 type2)
  (if (hash-has-key? coercion-table (list type1 type2))
      (hash-ref coercion-table (list type1 type2))
      #f))

(put-coercion 'scheme-number 'complex scheme-number->complex)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	  (apply proc (map contents args))
	  (if (= (length args) 2)
	      (let ((type1 (car type-tags))
		    (type2 (cadr type-tags))
		    (a1 (car args))
		    (a2 (cadr args)))
		(let ((t1->t2 (get-coercion type1 type2))
		      (t2->t1 (get-coercion type2 type1)))
		  (cond (t1->t2
			 (apply-generic op (t1->t2 a1) a2))
			(t2->t1
			 (apply-generic op a1 (t2->t1 a2)))
			(else
			 (error "No method for these types"
				(list op type-tags))))))
	      (error "No method for these types"
		     (list op type-tags)))))))
