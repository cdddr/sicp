;;; Introduction to Data Abstraction

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
