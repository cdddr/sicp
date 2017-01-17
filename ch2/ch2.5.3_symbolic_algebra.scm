;; Restricted to polynomials having one indeterminate.
;; polynomial - a sum of terms, each of which is either a coefficient, a power of the indeterminate, or a prodcut of the two.
;; coefficient - an algebraic expression that is not dependent on the indeterminate of the polynomial.
;; combining polynomials - to be combined, two polynomials must contain the same indeterminate.
;; Add support for complex numbers to use any lower number type for real or imag arguments

(define (square x) (mul x x))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (attach-tag type-tag contents)
  (if (number? contents)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (cond ((number? datum) 'scheme-number)
	    (else
	     #f))))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (cond ((number? datum) datum)
	    ((symbol? datum)
	     (error "Bad tagged datum -- CONTENTS" datum)))))


(define op-table (make-hash))
(define (put op type item)
  (hash-set! op-table (list op type) item))
(define (get op type)
  (if (hash-has-key? op-table (list op type))
      (hash-ref op-table (list op type))
      #f))

(define tower-level (make-hash))
(define (put-tower-level type level)
  (hash-set! tower-level type level))
(define (get-tower-level item)
  (if (hash-has-key? tower-level (type-tag item))
      ((hash-ref tower-level (type-tag item)) item)
      #f))

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (define (raise x)
    (cond ((exact-integer? x) (make-rational x 1))
	  (else
	   (make-complex-from-real-imag x 0))))
  
  (define (project x)
    (cond ((exact-integer? x) x)
	  (else
	   (inexact->exact (round x)))))
  (define (reduce-integers n d)
    (let ((g (gcd n d)))
      (list (/ n g) (/ d g))))

  (put 'add '(scheme-number scheme-number) +)
  (put 'sub '(scheme-number scheme-number) -)
  (put 'mul '(scheme-number scheme-number) *)
  (put 'div '(scheme-number scheme-number) /)
  (put 'equ? '(scheme-number scheme-number) =)		;just uses the built-in = for numbers
  (put '=zero? '(scheme-number) (lambda (x) (= x 0)))
  (put 'exp '(scheme-number scheme-number) expt)
  (put 'raise '(scheme-number) raise)
  (put 'project '(scheme-number) project)
  (put-tower-level 'scheme-number (lambda (x)
				    (cond ((exact-integer? x) 1)
					  (else
					   3))))
  (put 'cosine '(scheme-number) cos)
  (put 'sine '(scheme-number) sin)
  (put 'square-root '(scheme-number) sqrt)
  (put 'arctan '(scheme-number) atan)
  (put 'arctan '(scheme-number scheme-number) atan)
  (put 'greatest-common-divisor '(scheme-number scheme-number) gcd)
  (put 'reduce '(scheme-number scheme-number) reduce-integers)
  'done)

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cadr x))
  (define (make-rat n d)
    (reduce n d))


  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
		   (mul (numer y) (denom x)))
	      (mul (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (sub (mul (numer x) (denom y))
		   (mul (numer y) (denom x)))))
  (define (mul-rat x y)
    (make-rat (mul (numer x) (numer y))
	      (mul (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (mul (numer x) (denom y))
	      (mul (denom x) (numer y))))

  (define (rat-equ? x y)
    (and (= (numer x) (numer y))
	 (= (denom x) (denom y))))
  (define (=zero? x)
    (= (numer x) 0))
  (define (raise x)
    (exact->inexact (/ (numer x) (denom x))))
  (define (project x)
    (numer x))
  (define (cosine x)
    (cos (div (numer x) (denom x))))
  (define (sine x)
    (sin (div (numer x) (denom x))))
  (define (square-root x)
    (sqrt (div (numer x) (denom x))))
  (define (arctan y x)
    (atan (div (numer y) (denom y)) (div (numer x) (denom x))))

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
  (put 'equ? '(rational rational) rat-equ?)
  (put '=zero? '(rational) =zero?)
  (put 'raise '(rational) raise)
  (put 'project '(rational) project)
  (put-tower-level 'rational (lambda (x) 2))
  (put 'cosine '(rational) cosine)
  (put 'sine '(rational) sine)
  (put 'square-root '(rational) square-root)
  (put 'arctan '(rational rational) arctan)
  'done)

(define (install-rectangular-package)
  (define (real-part z)  (car z))
  (define (imag-part z)  (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (square-root (add (square (real-part z))
		      (square (imag-part z)))))
  (define (angle z)
    (arctan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (mul r (cosine a)) (mul r (sine a))))
  (define (rect-equ? z1 z2)
    (and (equ? (real-part z1) (real-part z2))
	 (equ? (imag-part z1) (imag-part z2))))
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
  (put 'equ? '(rectangular rectangular) rect-equ?)
  'done)

(define (install-polar-package)
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (mul (magnitude z) (cosine (angle z))))
  (define (imag-part z)
    (mul (magnitude z) (sine (angle z))))
  (define (make-from-real-imag x y)
    (cons (square-root (add (square x) (square y)))
	  (arctan y x)))
  (define (polar-equ? z1 z2)
    (and (equ? (magnitude z1) (magnitude z2))
	 (equ? (angle z1) (angle z2))))

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
  (put 'equ? '(polar polar) polar-equ?)
  'done)

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))

  (define (add-complex z1 z2)
    (make-from-real-imag (add (real-part z1) (real-part z2))
			 (add (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (sub (real-part z1) (real-part z2))
			 (sub (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (mul (magnitude z1) (magnitude z2))
		       (add (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (div (magnitude z1) (magnitude z2))
		       (sub (angle z1) (angle z2))))
  (define (complex-equ? z1 z2)
    (or (and (equ? (real-part z1) (real-part z2))
	     (equ? (imag-part z1) (imag-part z2)))
	(and (equ? (magnitude z1) (magnitude z2))
	     (equ? (angle z1) (angle z2)))))
  (define (=zero? z)
    (equ? z (make-complex-from-real-imag 0 0)))
  (define (raise x)
    (make-polynomial 'x (list (list 0 (tag x)))))
  (define (project x)
    (real-part x))
  
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
  (put 'equ? '(complex complex) complex-equ?)   ;added change for this exercise.
  (put '=zero? '(complex) =zero?)	;added for exercise 2.80.
  (put 'project '(complex) project)
  (put 'raise '(complex) raise)
  (put-tower-level 'complex (lambda (x) 4))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))


(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	  (apply proc (map contents args))
	  (if (and (= (length args) 2) (not (eq? (car type-tags) (cadr type-tags)))) ;Exericise 2.81 c)
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

(define (successive-raise item n)	
  (define (iter x i)
    (if (> i n)
	x
	(iter (raise x) (+ i 1))))
  (iter item 1))

(define (drop x)
  (cond	((and (pair? x) (> (length x) 1)) 
	 x)
	((eq? (type-tag x) 'polynomial) x)
	((not (type-tag x)) x)
	((= (get-tower-level x) 1) x)
	((equ? (raise (project x)) x)
	 (drop (project x)))
	(else
	 x)))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	  (if (or (eq? op 'raise) (eq? op 'project)) 
	      (apply proc (map contents args))
	      (apply proc (map contents args)))
	  (if (and (= (length args) 2) (not (eq? (car type-tags) (cadr type-tags)))) ;Exericise 2.81 c)
	      (let ((a1 (car args))
		    (a2 (cadr args)))
		(let ((tdiff (- (get-tower-level a1) (get-tower-level a2))))
		  (cond ((= tdiff 0) (apply-generic op a1 a2))
			((> tdiff 0) (apply-generic op a1 (successive-raise a2 tdiff)))
			((< tdiff 0) (apply-generic op (successive-raise a1 (abs tdiff)) a2)))))
	      (error "No op found for these types -- APPLY-GENERIC" (list op type-tags args)))))))

;; polynomial data structure - poly
(define (install-polynomial-package)
  (define (make-poly variable term-list)
    (if (null? term-list)
	(cons variable '((0 0)))
	(cons variable term-list)))
  ;; (define (make-poly variable term-list)
  ;;   (cons variable (if (cons)
  ;; 		       term-list
  ;; 		       (terms-dense->sparse term-list))))
  (define (terms-dense->sparse term-list)
    (define (iter lst result)
      (if (null? lst)
      	  result
          (iter (cdr lst) (append result (list (make-term (- (length lst) 1) (car lst)))))))
    (iter term-list '()))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (project p)
    (coeff (zero-term (term-list p))))
  (define (raise p)
    p)

  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
	(make-poly (variable p1)
		   (add-terms (term-list p1)
			      (term-list p2)))
	(error "Polys not in same var -- ADD-POLY" (list p1 p2))))
  (define (sub-poly p1 p2)
    (add-poly p1 (negate-poly p2)))
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
	(make-poly (variable p1)
		   (mul-terms (term-list p1)
			      (term-list p2)))
	(error "Polys not in same var -- MUL-POLY" (list p1 p2))))
  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
	(map (lambda (L)
	       (make-poly (variable p1) L))
	     (div-terms (term-list p1) (term-list p2)))
	(error "Polys not in the same var -- DIV-POLY" (list p1 p2))))  
  (define (negate-poly p)
    (make-poly (variable p)
	       (map (lambda (term)
		      (make-term (order term) (mul -1 (coeff term))))
		    (term-list p))))  
  (define (poly-=zero? p)
    (and (equ? (order (first-term (term-list p))) 0)
	 (equ? (coeff (first-term (term-list p))) 0)))
  (define (gcd-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
	(make-poly (variable p1) (gcd-terms (term-list p1) (term-list p2)))
	(error "Polys are not in the same var -- GCD-POLY" (list p1 p2))))
  (define (reduce-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
	(map (lambda (L)
	       (make-poly (variable p1) L)) (reduce-terms (term-list p1) (term-list p2)))
	(error "Polys are not in the same var -- REDUCE-POLY" (list p1 p2))))
  
  (define (the-empty-termlist) '())
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
    	term-list
    	(cons term term-list)))
  (define (equ?-poly p1 p2)
    (cond ((and (null? p1) (null? p2)) #t)
    	  ((or (and (null? p1) (not (null? p2)))
    	       (and (null? p2) (not (null? p1))))
    	   #f)
    	  (else
	   (let ((t1 (term-list p1)) (t2 (term-list p2)))
	     (and (eq? (variable p1) (variable p2))
		  (and (equ? (order (first-term t1)) (order (first-term t2)))
		       (equ? (coeff (first-term t1)) (coeff (first-term t2))))
		  (equ?-poly (rest-terms t1) (rest-terms t2)))))))
  (define (empty-termlist? term-list) (null? term-list))
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (zero-term term-list)
    (if (null? (cdr term-list))
	(if (equ? (order (car term-list)) 0)
	    (car term-list)
	    (make-term 0 0))
	(zero-term (cdr term-list))))
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
    	  ((empty-termlist? L2) L1)
    	  (else
	   (let ((t1 (first-term L1)) (t2 (first-term L2)))
	     (cond ((> (order t1) (order t2))
		    (adjoin-term t1 (add-terms (rest-terms L1) L2)))
		   ((< (order t1) (order t2))
		    (adjoin-term t2 (add-terms L1 (rest-terms L2))))
		   (else
		    (adjoin-term
		     (make-term (order t1)
				(add (coeff t1) (coeff t2)))
		     (add-terms (rest-terms L1)
				(rest-terms L2)))))))))
  (define (sub-terms L1 L2)
    (add-terms L1 (mul-terms '((0 -1)) L2)))
  
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
    	(the-empty-termlist)
    	(add-terms (mul-term-by-all-terms (first-term L1) L2)
		   (mul-terms (rest-terms L1) L2))))
  
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
	(the-empty-termlist)
	(let ((t2 (first-term L)))
	  (adjoin-term
	   (make-term (+ (order t1) (order t2))
		      (mul (coeff t1) (coeff t2)))
	   (mul-term-by-all-terms t1 (rest-terms L))))))

  (define (div-terms L1 L2)
    (if (empty-termlist? L1)
	(list (the-empty-termlist) (the-empty-termlist))
	(let ((t1 (first-term L1))
	      (t2 (first-term L2)))
	  (if (> (order t2) (order t1))
	      (list (the-empty-termlist) L1)
	      (let ((new-c (div (coeff t1) (coeff t2)))
		    (new-o (sub (order t1) (order t2))))
		(let ((rest-of-result
		       (div-terms (sub-terms L1
					     (mul-terms (list (make-term new-o new-c))
							L2))
				  L2)))
		  (list (adjoin-term (make-term new-o new-c) (car rest-of-result)) (cadr rest-of-result))))))))
  
  (define (gcd-terms a b)
    (if (empty-termlist? b)
    	a
    	(reduce-termlist (gcd-terms b (psuedoremainder-terms a b)))))
  (define (remainder-terms a b)
    (cadr (div-terms a b)))
  (define (psuedoremainder-terms p q)
    (let ((o1 (order (first-term p)))
	  (o2 (order (first-term q)))
	  (c  (coeff (first-term q))))
      (cadr (div-terms (mul-terms (list (list 0 (expt c (+ 1 (- 4 3))))) p) q))))
  
  (define (reduce-termlist terms . factor)
    (let ((orders (map (lambda (x) (order x)) terms))
	  (coeffs (map (lambda (x) (coeff x)) terms)))
      (let ((g (if (null? factor)
		   (apply gcd coeffs)
		   (car factor))))
	(zip-order-coeff orders (reduce-coeffs coeffs g)))))
  (define (reduce-coeffs coeffs factor)
    (map (lambda (x) (div x factor)) coeffs))
  (define (zip-order-coeff os cs)
    (if (null?  os)
	'()
	(cons (list (car os) (car cs)) (zip-order-coeff (cdr os) (cdr cs)))))
  (define (unzip-coeff terms)
    (map (lambda (x) (coeff x)) terms))

  (define (reduce-terms n d)
    (let ((terms-gcd (gcd-terms n d)))
      (let ((o1 (max (order (first-term n)) (order (first-term d))))
	    (o2 (order (first-term terms-gcd)))
	    (c (coeff (first-term terms-gcd))))
	(let ((factor (expt c (+ 1 (- o1 o2)))))
	  (let ((nn (car (div-terms (mul-terms (list (list 0 factor)) n) terms-gcd)))
		(dd (car (div-terms (mul-terms (list (list 0 factor)) d) terms-gcd))))
	    (let ((coeffs-gcd (apply gcd (append (unzip-coeff nn) (unzip-coeff dd)))))
	      (list (reduce-termlist nn coeffs-gcd) (reduce-termlist dd coeffs-gcd))))))))
  
  
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'add '(scheme-number polynomial)
       (lambda (n p) (tag (add-poly (make-poly (variable p) (list (list 0 n))) p))))
  (put 'add '(rational polynomial)
       (lambda (n p) (tag (add-poly (make-poly (variable p) (list (list 0 n))) p))))  
  (put 'add '(complex polynomial)
       (lambda (n p) (tag (add-poly (make-poly (variable p) (list (list 0 n))) p))))  
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'sub '(scheme-number polynomial)
       (lambda (n p) (tag (sub-poly (make-poly (variable p) (list (list 0 n))) p))))
  (put 'sub '(rational polynomial)
       (lambda (n p) (tag (sub-poly (make-poly (variable p) (list (list 0 n))) p))))  
  (put 'sub '(complex polynomial)
       (lambda (n p) (tag (sub-poly (make-poly (variable p) (list (list 0 n))) p))))  
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'mul '(scheme-number polynomial)
       (lambda (n p) (tag (mul-poly (make-poly (variable p) (list (list 0 n))) p))))  
  (put 'mul '(rational polynomial)
       (lambda (n p) (tag (mul-poly (make-poly (variable p) (list (list 0 n))) p))))  
  (put 'mul '(complex polynomial)
       (lambda (n p) (tag (mul-poly (make-poly (variable p) (list (list 0 n))) p))))
  (put 'div '(polynomial polynomial)
       (lambda (p1 p2) (map tag (div-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  (put-tower-level 'polynomial (lambda (x) 5))
  (put '=zero? '(polynomial) poly-=zero?)
  (put 'negate '(polynomial) (lambda (x) (tag (negate-poly x))))
  (put 'equ? '(polynomial polynomial) equ?-poly)
  (put 'project '(polynomial) project)
  (put 'raise '(polynomial) raise)
  (put 'greatest-common-divisor '(polynomial polynomial)
       (lambda (p1 p2) 
	 (tag (gcd-poly p1 p2))))
  (put 'reduce '(polynomial polynomial)
       (lambda (p1 p2)
	 (map tag (reduce-poly p1 p2))))
  'done)

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))
(define (raise x) (apply-generic 'raise x))
(define (project x) (apply-generic 'project x))
(define (cosine x) (apply-generic 'cosine x))
(define (sine x) (apply-generic 'sine x))
(define (square-root x) (apply-generic 'square-root x))
(define (arctan y x) (apply-generic 'arctan y x))  	
(define (negate x) (apply-generic 'negate x))
(define (greatest-common-divisor x y) (apply-generic 'greatest-common-divisor x y))
(define (reduce x y) (apply-generic 'reduce x y))

(install-scheme-number-package)
(install-rational-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)
(install-polynomial-package)

;; OK, this is officially neat.
;;> (add (make-polynomial 'x '((5 1) (4 2) (2 3) (1 -2) (0 -5)))
;;       (make-polynomial 'x '((0 10))))
;;'(polynomial x (5 1) (4 2) (2 3) (1 -2) (0 5))

;; Exercise 2.87 =zero? for polynomials. Implemented above.
;;> x-poly-1
;;'(polynomial x (1 (polynomial y (1 2) (0 1))) (0 1))
;;> x-poly-2
;;'(polynomial x (1 (polynomial y (1 3) (0 5))) (0 5))
;;> (add x-poly-1 x-poly-2)
;;'(polynomial x (1 (polynomial y (1 5) (0 6))) (0 6))

;; Exercise 2.88 - extend the system to include subtraction.
;; It's working, it's definitely not as general as it could be. I initially tried to coerce types by
;; making polynomials level 5 of the tower, but just doing a plain raise does not work because you don't know if the 
;; variable names are going to match. So, instead I just put operations in the op-table for number/rational/complex & polynomial.
;; Then I can build a polynomial based on the variable that is in the other operand.
;;> (sub x-poly-1 x-poly-2)
;;'(polynomial x (1 (polynomial y (1 -1) (0 -4))) (0 -4))
;;> x-poly-1
;;'(polynomial x (1 (polynomial y (1 2) (0 1))) (0 1))
;;> x-poly-2
;;'(polynomial x (1 (polynomial y (1 3) (0 5))) (0 5))

;; Exercise 2.89 - dense polynomials.
;; Took the easy way out for now and just have the constuctor check for pairs in term list. If a pair is found use it as is, otherwise 
;; convert the list from dense -> sparse assuming that the list given is in the correct order.
;;> (add 4 (make-polynomial 'x '(2 1 2)))
;;'(polynomial x (2 2) (1 1) (0 6))

;; Exercise 2.90 - skipping this for now. Will require a pretty large amount of work.

;; Exercise 2.91 - Division of Polynomials

;; racket@> (div (make-polynomial 'x '((5 1) (0 -1))) (make-polynomial 'x '((2 1) (0 -1))))
;; '((polynomial x (3 1) (1 1)) (polynomial x (1 1) (0 -1)))

;; Exercise 2.92 - [TODO - qdbp_] Skipping for now


;; Exercise 2.93
;;> (add rf rf)
;;'(rational
;;  (polynomial x (5 2) (3 2) (2 2) (0 2))
;;  (polynomial x (4 1) (2 2) (0 1)))

;;; Exercise 2.94 - Changes Above for greatest-common-divisor

;; racket@> (greatest-common-divisor p1 p2)
;; '(polynomial x (2 -1) (1 1))
;;; Checks out with a hand worked long division and recursion of gcd.

;;; Exercise 2.95 - 
;; racket@> (define p1 (make-polynomial 'x '((2 1) (1 -2) (0 1))))
;; racket@> (define p2 (make-polynomial 'x '((2 11) (0 7))))
;; racket@> (define p3 (make-polynomial 'x '((1 13) (0 5))))
;; racket@> (define q1 (mul p1 p2))
;; racket@> (define q2 (mul p1 p3))
;; racket@> q1
;; '(polynomial x (4 11) (3 -22) (2 18) (1 -14) (0 7))
;; racket@> q2
;; '(polynomial x (3 13) (2 -21) (1 3) (0 5))
;; racket@> (greatest-common-divisor q1 q2)
;; '(polynomial x (2 1458/169) (1 -2916/169) (0 1458/169))
;; The division algorithm introduces some unreducible divisions that get fixed in the first term, but cause
;; problems when it is multiplied through in the remainder step.

;; racket@> (greatest-common-divisor q1 q2)
;; gcd-terms ((4 11) (3 -22) (2 18) (1 -14) (0 7)),((3 13) (2 -21) (1 3) (0 5))
;; gcd-terms ((3 13) (2 -21) (1 3) (0 5)),((2 1458/169) (1 -2916/169) (0 1458/169))
;; gcd-terms ((2 1458/169) (1 -2916/169) (0 1458/169)),()
;; '(polynomial x (2 1458/169) (1 -2916/169) (0 1458/169))

;; Exercise 2.96 - pseudoremainder implemented above.
;; a)
;; racket@> (greatest-common-divisor q1 q2)
;; '(polynomial x (2 1458) (1 -2916) (0 1458))
;; b)
;; racket@> (greatest-common-divisor q1 q2)
;; '(polynomial x (2 1) (1 -2) (0 1))

;;; Exercise 2.97
;; (define p1 (make-polynomial 'x '((1 1) (0 1))))
;; (define p2 (make-polynomial 'x '((3 1) (0 -1))))
;; (define p3 (make-polynomial 'x '((1 1))))
;; (define p4 (make-polynomial 'x '((2 1) (0 -1))))

;; (define rf1 (make-rational p1 p2))
;; (define rf2 (make-rational p3 p4))

;; racket@> rf1
;; '(rational (polynomial x (1 -1) (0 -1)) (polynomial x (3 -1) (0 1)))
;; racket@> rf2
;; '(rational (polynomial x (1 1)) (polynomial x (2 1) (0 -1)))
;; racket@> (add rf1 rf2)
;; '(rational
;;   (polynomial x (3 -1) (2 -2) (1 -3) (0 -1))
;;   (polynomial x (4 -1) (3 -1) (1 1) (0 1)))
;; racket@> (add rf3 rf4)
;; '(rational (polynomial x (1 1) (0 1)) (polynomial x (2 1) (1 -1)))
;; racket@> rf3
;; '(rational (polynomial x (0 1)) (polynomial x (2 1) (1 -1)))
;; racket@> rf4
;; '(rational (polynomial x (0 1)) (polynomial x (1 1) (0 -1)))
