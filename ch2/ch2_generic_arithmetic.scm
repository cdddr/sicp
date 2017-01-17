(define (square x) (* x x))

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
  (define (raise x)
  	(exact->inexact (/ (numer x) (denom x))))
  (define (project x)
  	(numer x))

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
  (put 'raise '(rational) raise)
  (put 'project '(rational) project)
  (put-tower-level 'rational (lambda (x) 2))
  'done)

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
    (or (and (= (real-part z1) (real-part z2))
	 		(= (imag-part z1) (imag-part z2)))
	 	(and (= (magnitude z1) (magnitude z2))
	 		 (= (angle z1) (angle z2)))))
  (define (=zero? z)
    (equ? z (make-complex-from-real-imag 0 0)))
  (define (raise x)
  	x)
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
  (put 'equ? '(complex complex) equ?)   ;added change for this exercise.
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

(install-scheme-number-package)
(install-rational-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)


;;; Exercise 2.81
;; a)
(define (scheme-number->scheme-number n) n)
(define (complex->complex z) z)
(put-coercion 'scheme-number 'scheme-number
	      scheme-number->scheme-number)
(put-coercion 'complex 'complex complex->complex)

(define (exp x y) (apply-generic 'exp x y))

;;; program goes into an infinite loop because it never finds an op for exp in the complex package, but the
;;; coercions exist. This causes apply-generic to go into an infinite loop.

;; b) I don't think anything has to be done about arguments of the same type if the op exists. If the op doesn't
;;    exist it is probably worth adding the defensive code in.

;; c) See the tagged section of apply-generic above

;;; Exercise 2.82
;; How to generalize apply-generic to handle coercion in the general case of multiple arguments.
;;(define (apply-generic op . args)
;;  (let ((coerced-args (coerce-list args)))
;;    (let ((type-tags (map type-tag coerced-args)))
;;  		(display coerced-args)(newline)
;;  		(display type-tags)(newline)
;;  		(let ((proc (get op type-tags)))
;;      	(if proc
;;      		(apply proc (map contents coerced-args))
;;      		(error 
;;      			"Unable to find a valid procedure -- APPLY-GENERIC" (list op args coerced-args)))))))
;;      			
;;(define (add . args) (apply-generic 'add args))		     
;;;; Strategies - 
;;;;  1) Coerce all arguments to the first type in the list, then the second, and so on.
;;(define (coerce-list-to-type item lst)
;;	(map (lambda (x)
;;			(let ((coercion (get-coercion (type-tag x) (type-tag item))))
;;				(if coercion
;;					(coercion x)
;;					x)))
;;		lst))
;;
;;(define (coerce-list args)
;;	(define (iter lst coercions)
;;		(display lst)(display " : ")(display coercions)(newline)
;;		(if (null? lst)
;;			coercions
;;			(iter (cdr lst) (coerce-list-to-type (car lst) coercions))))
;;	(iter args args))  

;; This seems less than ideal and with a large argument list will be doing a lot of coercion since it goes through n elements for each n
;; element (n^2). Also, since this method coerces everything first any operations that already support type->type operations in the 
;; dispatch table will not be used.

;;; Exercise 2.83

;; To raise an integer to a rational, divide by one
;; (define (raise integer)
;;   (make-rational integer 1))
;; To raise a rational to a real number ... assume we have a make-real constructor for however they would be represented.
;; Could just divide the two, but will result in a loss of precision.
;; (define (raise rational)
;;   (make-real rational)) ;; or ;; (make-real (/ (numer rational) (denom rational))) maybe
;; To raise a real to a complex number, just add 0i onto it
;; (define (raise real)
;;   (make-complex-from-real-imag real 0))

;; Exercise 2.84
(define (lower-type? i j)
  (< (get-tower-level i) (get-tower-level j)))

(define (successive-raise item n)	
  (define (iter x i)
 	(if (> i n)
			x
			(iter (raise x) (+ i 1))))
	(iter item 1))

(define (drop x)
  (cond ((not (type-tag x)) x)
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
	   		(drop (apply proc (map contents args))))
	    (if (and (= (length args) 2) (not (eq? (car type-tags) (cadr type-tags)))) ;Exericise 2.81 c)
	  	  (let ((a1 (car args))
	  	  		(a2 (cadr args)))
	  	  	(let ((tdiff (- (get-tower-level a1) (get-tower-level a2))))
	   	  		(cond ((= tdiff 0) (apply-generic op a1 a2))
	  	  			  ((> tdiff 0) (apply-generic op a1 (successive-raise a2 tdiff)))
	  	  			  ((< tdiff 0) (apply-generic op (successive-raise a1 (abs tdiff)) a2)))))
	  	  (error "No op found for these types -- APPLY-GENERIC" (list op type-tags)))))))
	  	  
;;> (add 1.5 (make-rational 3 2))
;;3.0
;;> (add 2.5 1)
;;3.5
;;> (add 1 2.0)
;;3.0
;;> (add 1 (make-rational 1 2))
;;'(rational 3 . 2)
;;> (add 1 (make-complex-from-real-imag 1 2))
;;'(complex rectangular 2.0 . 2)

;;; Exercise 2.85
;; The project functions have been added to the type packages above.
;; The way I designed/implemented (tower-level) previously put me in an infinite loop situation because
;; I'm trying put drop in apply-generic, but apply-generic dispatches tower-level, so it goes into an infinite loop if I call
;; tower-level. I'm going to reimplement tower-level as a top-level function that does a lookup for the type to get its level.
;; Due to how the scheme-number package determines its level based on exact-integer? get-tower-level gets a function and executes it
;; to return the level number.

;; Another infinite loop I was hitting was just doing a general drop when dispatching to project/raise. There is a guard against those
;; two operations, any other operation is dropped as low as it will go.
;;> (add (make-rational 7 2) (make-rational 7 2))
;;7
;;> (mul (make-rational 7 2) 2)
;;7
;;> (div (make-rational 7 2) 7)
;;'(rational 1 . 2)
;;> (div (make-complex-from-real-imag 25.0 1) 5)
;;'(complex polar 5.0039984012787215 . 0.039978687123290044)
;;> (div (make-complex-from-real-imag 25.0 0) 5)
;;5

