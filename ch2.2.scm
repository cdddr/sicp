;;; Hierarchical Data and the closure property

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

;;; Exercise 2.17 - last element of a list
(define (last-pair list1)
  (if (null? (cdr list1))
      (cons (car list1) '())
      (last-pair (cdr list1))))

;;; Exercise 2.18 - reverse of a list
(define (reverse list1)
  (define (reverse-iter list2 revlist)
    (if (null? list2)
	revlist
	(reverse-iter (cdr list2) (cons (car list2) revlist))))
  (reverse-iter list1 '()))

;;; Exercise 2.19
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

;;; from ch1.scm:57
(define count-change-calls 0)
(define (count-change amount)
  (set! count-change-calls 0)
  (print (cc amount 5))
  (print "steps : " count-change-calls))

(define (cc amount coin-values)
  (set! count-change-calls (+ count-change-calls 1))
  (cond ((= amount 0) 1)
	((or (< amount 0) (no-more? coin-values)) 0)
	(else (+ (cc amount
		     (except-first-denomination coin-values))
		 (cc (- amount
			(first-denomination coin-values))
		     coin-values)))))
(define (first-denomination coin-values)
  (car coin-values))
(define (except-first-denomination coin-values)
  (cdr coin-values))
(define (no-more? coin-values)
  (null? coin-values))

;;; Order does not seem to matter.
;; racket@> (cc 100 us-coins)
;; 292
;; racket@> (cc 100 (list 1 5 10 25 50))
;; 292
;; racket@> (cc 100 uk-coins)
;; 104561
;; racket@> (cc 100 (list 1 10 5 50 25))
;; 292
;; racket@> 

;;; Exercise 2.20
(define (same-parity parity . ints)
  (define (same-parity-iter parity list1 paritylist)
    (if (null? list1)
	paritylist
	(same-parity-iter parity (cdr list1) (if (= (modulo (car list1) 2) parity)
							     (append paritylist (list (car list1)))
							     paritylist))))
  (same-parity-iter (modulo parity 2) (cons parity ints) '()))

;; racket@> (same-parity 1 2 3 4 5 6 7)
;; '(1 3 5 7)
;; racket@> (same-parity 2 3 4 5 6 7 8)
;; '(2 4 6 8)

(define (map proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
	    (map proc (cdr items)))))

(define (scale-list items factor)
  (map (lambda (x) (* x factor))
       items))

;;; Exercise 2.21 - square-list two ways
(define (square-list items)
  (if (null? items)
      '()
      (cons (* (car items) (car items)) (square-list (cdr items)))))

(define (square-list items)
  (map (lambda (x) (* x x)) items))

;;; Exercise 2.22
;; The first definition produces the list in reverse order because it cons'ing onto the front of the list.
;; So, as items are popped off (1 2 3 4), it ends up expanding into
;;  (cons 16 (cons 9 (cons 4 (cons 1 null))))
;; racket@> (cons 16 (cons 9 (cons 4 (cons 1 null))))
;; '(16 9 4 1)

;; Interchanging the arguments to cons doesn't work either, because answer is a list, so it is cons'ing a list
;; onto the list, not appending the lists together.
;; Using the same example (1 2 3 4) :
;;  (cons '() 1)
;;  (cons (() 1) 4)
;;  (cons ((() 1) 4) 9)
;;  (cons (((() 1) 4) 9) 16)

(define (square-list-louis-2 items)
  (define (square x)
    (* x x))
  (define (iter things answer)
    (if (null? things)
	answer
	(iter (cdr things)
	      (cons answer
		    (square (car things))))))
  (iter items '()))

;; racket@> (square-list-louis-2 (list 1 2 3 4))
;; '((((() . 1) . 4) . 9) . 16)
;;; using append would work with this way of doing things
(define (square-list-louis-3 items)
  (define (square x)
    (* x x))
  (define (iter things answer)
    (if (null? things)
	answer
	(iter (cdr things)
	      (append answer
		      (list (square (car things)))))))
  (iter items (list)))
;; racket@> (square-list-louis-3 (list 1 2 3 4))
;; '(1 4 9 16)

;;; Exercise 2.23
(define (for-each proc things)
  (if (null? things)
      #t
      (proc (car things)))
  (for-each proc (cdr things)))

;; racket@> (for-each (lambda (x) (display x) (newline)) (list 57 321 88))
;; 57
;; 321
;; 88

;;; 2.2.2 Hierarchical Structures

(define (count-leaves x)
  (cond ((null? x) 0)
	((not (pair? x)) 1)
	(else (+ (count-leaves (car x))
		 (count-leaves (cdr x))))))

;;; Exercise 2.24
;; Suppose (list 1 (list 2 (list 3 4))) is evalued. What is printed by the interpreter? Box & Pointer?

;; '(1 (2 (3 4)))
;; Check:
;; racket@> (list 1 (list 2 (list 3 4))) 
;; '(1 (2 (3 4)))

;; Box and Pointer in notebook

;;; Exercise 2.25

;; pick 7 from lists with car/cdr

;; (1 3 (5 7) 9)
;; (car (cdaddr 
;; racket@> (car (cdaddr (list 1 3 (list 5 7) 9)))
;; 7

;;((7))
;; (caar
;; racket@> (caar (list (list 7)))
;; 7

;; (1 (2 (3 (4 (5 (6 7))))))
;; cadr - 6 times
;; racket@> (cadr (cadr  (cadr  (cadr  (cadr (cadr (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7))))))))))))
;; 7

;;; Exercise 2.25

;; (define x (list 1 2 3))
;; (define y (list 4 5 6))

;; (append x y) -> (1 2 3 4 5 6)

;; (cons x y) -> ((1 2 3) 4 5 6)

;; (list x y) -> ((1 2 3) (4 5 6))

;; racket@> (define x (list 1 2 3))
;; racket@> (define y (list 4 5 6))
;; racket@> (append x y)
;; '(1 2 3 4 5 6)
;; racket@> (cons x y)
;; '((1 2 3) 4 5 6)
;; racket@> (list x y)
;; '((1 2 3) (4 5 6))

;;; Exercise 2.27

(define (deep-reverse lst)
  (define (iter lst revlst)
    (cond ((null? lst) revlst)
	  ((pair? (car lst))
	   (iter (cdr lst) (cons  (deep-reverse (car lst)) revlst)))
	  (else
	   (iter (cdr lst) (cons (car lst) revlst)))))
  (iter lst '()))

;; racket@> x
;; '((1 2) (3 4) (5 (6 7)))
;; racket@> (deep-reverse x)
;; '(((7 6) 5) (4 3) (2 1))

;;; Exercise 2.28
;; TODO : I'm not quite sure how to do this using pure recursion yet. The recursive version I initially  wrote out is
;;        is preserving the list structure, not flattening tree.
(define (fringe tree)
  (define leaves '())
  (define (iter lst)
    (cond ((null? lst) leaves)
	  ((not (pair? lst)) (set! leaves (cons lst leaves)))
	  (else
	   (iter (car lst))
	   (iter (cdr lst)))))
  (iter (deep-reverse tree)))

;; racket@> (fringe x)
;; '(1 2 3 4)
;; racket@> (fringe (list x x))
;; '(1 2 3 4 1 2 3 4)


;; iterative-recursive might be the way to do this
;; I tried something similar to this earlier, but I was trying to use a pure iterative way of doing things
;; and the state variable (flatlst) was not carrying its state throughout the whole tree. By using a combination
;; of iteration and recursion, the desired result is obtained. Since it recurses through the flatlst portion, the
;; conses take place in the correct (left -> right) order.
(define (fringe-1 tree)
  (define (iter tree flatlst)
    (cond ((null? tree) flatlst)
	  ((not (pair? tree)) (cons tree flatlst))
	  (else
	   (iter (car tree)
		 (iter (cdr tree) flatlst)))))
  (iter tree '()))

;; racket@> (fringe-1 x)
;; '(1 2 3 4)
;; racket@> (fringe-1 (list x x))
;; '(1 2 3 4 1 2 3 4)
;; racket@> (fringe-1 (list 1 (list 2 3 (list 4 5 6 (list 7 8 9))) (list 10 11)))
;; '(1 2 3 4 5 6 7 8 9 10 11)

;;; Exercise 2.29
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))

;; racket@> (left-branch (make-mobile (make-branch 1 2.0) (make-branch 2 3.0)))
;; '(1 2.0)
;; racket@> (right-branch (make-mobile (make-branch 1 2.0) (make-branch 2 3.0)))
;; '(2 3.0)
;; racket@> (right-branch (make-mobile (make-branch 1 2.0) (make-branch 2 (make-mobile (make-branch 3 1) (make-branch 4 1)))))
;; '(2 ((3 1) (4 1)))
;; racket@> (branch-length (right-branch (make-mobile (make-branch 1 2.0) (make-branch 2 (make-mobile (make-branch 3 1) (make-branch 4 1))))))
;; 2
;; racket@> (branch-structure (right-branch (make-mobile (make-branch 1 2.0) (make-branch 2 (make-mobile (make-branch 3 1) (make-branch 4 1))))))
;; '((3 1) (4 1))
;;; so far, so good.

(define (total-weight mobile)
  (define (iter mobile sum)
    (cond ((null? mobile) sum)
	  ((not (pair? mobile)) (+ mobile sum))
	  (else
	   (iter (branch-structure (left-branch mobile))
		 (iter (branch-structure (right-branch mobile)) sum)))))
  (iter mobile 0))


;;; We have two requirements :
;;;  1) The torque of each branch is equal to each other (length * weight)
;;;  2) Each sub-mobile must also be be balanced.

;;; We need to recurse through the entire tree to find the weight hanging off the top left and top right branches
;;; As we reach each mobile we must also keep track of each sub-mobiles balanced-ness. Should be able to keep track
;;; of a boolean value and if they call come back #t and the torques are equal, we are good.

;;; naive version...two passes. One to calculate the total torque on the top branches, one to ensure each sub-mobile
;;; is balanced.
(define (branch-torque branch)
  (* (branch-length branch) (total-weight (branch-structure branch))))

(define (balanced-mobile? mobile)
  (cond ((null? mobile) #t)
	((not (pair? mobile)) #t)
	(else
	 (and (=
	       (branch-torque (left-branch mobile))
	       (branch-torque (right-branch mobile)))
	      (balanced-mobile? (branch-structure (left-branch mobile)))
	      (balanced-mobile? (branch-structure (right-branch mobile)))))))

;;; This version is actually a multi pass approach, since I descend the tree at every sub-mobile to get the weight.
;;; Super inefficient, but it seems to work.

;;;                                            x
;;;                                           / \
;;;                                       1  /   \ 2
;;;                                         /     \
;;;                                       100      +
;;;                                               / \
;;;                                              /   \
;;;                                           4 /     \ 1
;;;                                            /       \
;;;                                           +         +
;;;                                          / \        | \
;;;                                       2 /   \ 3    3|  \ 1
;;;                                        /     \      |   \
;;;                                       6       4    10    30

(define test-mobile
  (make-mobile (make-branch 1 100)
	       (make-branch 2
			    (make-mobile (make-branch 4
						      (make-mobile (make-branch 2 6)
								   (make-branch 3 4)))
					 (make-branch 1
						      (make-mobile (make-branch 3 10)
								   (make-branch 1 30)))))))

;; racket@> (balanced-mobile? test-mobile)
;; #t

(define test-mobile-unequal-torque
  (make-mobile (make-branch 1 100)
	       (make-branch 3
			    (make-mobile (make-branch 4
						      (make-mobile (make-branch 2 6)
								   (make-branch 3 4)))
					 (make-branch 1
						      (make-mobile (make-branch 3 10)
								   (make-branch 1 30)))))))

;; racket@> (balanced-mobile? test-mobile-unequal-torque)
;; #f

(define test-mobile-unbalanced-submobile
  (make-mobile (make-branch 1 100)
	       (make-branch 2
			    (make-mobile (make-branch 4
						      (make-mobile (make-branch 2 6)
								   (make-branch 3 4)))
					 (make-branch 1
						      (make-mobile (make-branch 3 10)
								   (make-branch 2 30)))))))
;; racket@> (balanced-mobile? test-mobile-unbalanced-submobile)
;; #f

;;; This solution works, but is there a way to do a single pass?
;;; TODO : Not going to prematurely optimize this, but would be a cool problem to explore

;;; Suppose the new constructors are :

(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

;;; Q: what changes need to be made?
;;; A: none!

(define (scale-tree tree factor)
  (cond ((null? tree) '())
	((not (pair? tree)) (* tree factor))
	(else (cons (scale-tree (car tree) factor)
		    (scale-tree (cdr tree) factor)))))

;; #;83> (scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)) 10)
;; (10 (20 (30 40) 50) (60 70))

;;; Alternate implementation using map to scale each sub tree
(define (scale-tree tree factor)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	     (scale-tree sub-tree factor)
	     (* sub-tree factor)))
       tree))

;; #;114> (scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)) 10)
;; (10 (20 (30 40) 50) (60 70))

(define (square-tree tree)
  (cond ((null? tree) '())
	((not (pair? tree)) (* tree tree))
	(else (cons (square-tree (car tree))
		    (square-tree (cdr tree))))))

;; #;151> (square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))
;; (1 (4 (9 16) 25) (36 49))

(define (square-tree tree)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	     (square-tree sub-tree)
	     (* sub-tree sub-tree)))
       tree))

;; #;189> (square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))
;; (1 (4 (9 16) 25) (36 49))

(define (tree-map lmbdf tree)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	     (tree-map lmbdf sub-tree)
	     (lmbdf sub-tree)))
       tree))
(define (square-tree tree)
  (tree-map (lambda (x) (* x x)) tree))

;; #;193> (square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))
;; (1 (4 (9 16) 25) (36 49))

(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
	(append rest (map (lambda (x)
			    (cons (car s) x)) rest)))))

;; #;645> (subsets (list 1 2 3))
;; (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))
;; #;653> (subsets (list 1 2 3 4))
;; (() (4) (3) (3 4) (2) (2 4) (2 3) (2 3 4) (1) (1 4) (1 3) (1 3 4) (1 2) (1 2 4) (1 2 3) (1 2 3 4))

;;; This works by "mixing in" the car of s at each let binding.
;;; After recursion it ends up expanding like so:
;; (let ((rest
;;        (let ((rest
;; 	      (let ((rest
;; 		     (let ((rest '()))				s='() rest='()
;; 		       (append rest (map (lambda (x)
;;					     (cons (car s) x)) rest))))))))))))
;;;			this results in '(())
;;; normally (car '()) would be a problem, but it never gets executed becase rest is itself empty.
;;; unwinding the let bindings at the next step we now have
;; (let ((rest
;;        (let ((rest
;; 	      (let ((rest '(()) )))				s='(3) rest='(())
;; 		(append rest (map (lambda (x)  
;; 				    (cons (car s) x)) rest)))))))))
;;; this results in '(() '(3)). the next step will give '(() (3) (2) (2 3)) and so on.


;;; Section 2.2.3 - Sequences as Conventional Interfaces
;;; ----------------------------------------------------

;;; normal recursive definition of summing the odd squares of a tree
(define (square x)
  (* x x))

(define (sum-odd-squares tree)
  (cond ((null? tree) 0)
	((not (pair? tree))
	 (if (odd? tree) (square tree) 0))
	(else (+ (sum-odd-squares (car tree))
		 (sum-odd-squares (cdr tree))))))

;; #;36> (sum-odd-squares (list 1 (list 2 (list 3 4) 5) (list 6 7)))
;; 84

;;; Need some building blocks to move towards a signal processing representation

(define nil '())

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
	((predicate (car sequence))
	 (cons (car sequence)
	       (filter predicate (cdr sequence))))
	(else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
	((not (pair? tree)) (list tree))
	(else (append (enumerate-tree (car tree))
		      (enumerate-tree (cdr tree))))))


;; #;121> (accumulate + 0 (list 1 2 3 4 5))
;; 15
;; #;127> (accumulate * 1 (list 1 2 3 4 5))
;; 120
;; #;130> (accumulate cons nil (list 1 2 3 4 5))
;; (1 2 3 4 5)
;; #;135> (enumerate-interval 0 10)
;; (0 1 2 3 4 5 6 7 8 9 10)
;; #;160> (enumerate-tree (list 1 (list 2 (list 3 4)) 5))
;; (1 2 3 4 5)
;; #;192>

;;; Signal Processing style:

(define (sum-odd-squares tree)
  (accumulate + 0 (map square
		       (filter odd?
			       (emumerate-tree tree)))))

(define (fib n)
  (fib-linear-iter 1 0 n))

(define (fib-linear-iter a b count)
  (if (= count 0)
      b
      (fib-linear-iter (+ a b) a (- count 1))))


(define (even-fibs n)
  (accumulate cons nil (filter even?
			       (map fib
				    (enumerate-interval 0 n)))))

(define (list-fib-squares n)
  (accumulate cons nil
	      (map square
		   (map fib
			(enumerate-interval 0 n)))))

;; #;262> (list-fib-squares 10)
;; (0 1 1 4 9 25 64 169 441 1156 3025)

(define (product-of-squares-of-odd-elements seq)
  (accumulate * 1
	      (map square
		   (filter odd? seq))))


;; #;299> (product-of-squares-of-odd-elements (list 1 2 3 4 5))
;; 225

;;; Exercise 2.33
(define (my-map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

;; #;303> (map square (list 1 2 3 4))
;; (1 4 9 16)
;; #;327> (map square (list 1 2 3 4 5 6))
;; (1 4 9 16 25 36)
;; #;329> (map fib (list 1 2 3 4 5 6))
;; (1 1 2 3 5 8)

(define (append-my seq1 seq2)
  (accumulate cons seq2 seq1))

;; #;381> (append-my (list 1 2 3) (list 2 3 4))
;; (1 2 3 2 3 4)

(define (length sequence)
  (accumulate + 0 (map (lambda (x) 1) sequence)))

;; #;422> (length (list 1 2 3 4 5 6))
;; 6

;;; Exercise 2.34 - Horner's Rule

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
		(+ this-coeff (* x higher-terms)))
	      0
	      coefficient-sequence))


;; #;426> (horner-eval 2 (list 1 3 0 5 0 1))
;; 79

;;; Exercise 2.35 - Redefine count-leaves as an accumulation

(define (count-leaves-old x)
  (cond ((null? x) 0)
	((not (pair? x)) 1)
	(else (+ (count-leaves-old (car x))
		 (count-leaves-old (cdr x))))))

(define (count-leaves tree)
  (accumulate + 0 (map (lambda (x) 1)
		       (enumerate-tree tree))))

;; #;538> (count-leaves (list 1 (list 2 (list 3 4) 5 (list 6 7))))
;; 7


;;; Exercise 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
	    (accumulate-n op init (map cdr seqs)))))

;;; Exercise 2.37 - Matrix algebra
;;; Chicken doesn't seem to suppor the more general version of map. Rolled my own.
(define (dot-product v w)
  (accumulate + 0 (accumulate-n * 1 (list v w))))

;; #;119> (dot-product (list 1 2 3 4) (list 1 2 3 4))
;; 30

(define (matrix-*-vector m v)
  (map (lambda (w)
	 (dot-product v w)) m))

;; #;141> (matrix-*-vector (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)) (list -2 1 0))
;; (0 -3 -6 -9)

(define (transpose mat)
  (accumulate-n cons nil mat))

;; #;198> (transpose (list (list 11 12 13) (list 21 22 23) (list 31 32 33)))
;; ((11 21 31) (12 22 32) (13 23 33))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (mk)
	   (map (lambda (nk)				 ; this is matrix-*-vector
		  (dot-product mk nk)) cols)) m)))	 ;

;;; switched back to racket, b/c it works better with geiser it seems
;; racket@> (matrix-*-matrix (list (list 1 2 3) (list 4 5 6)) (list (list 1 2) (list 3 4) (list 5 6)))
;; '((22 28) (49 64))


(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (mk)
	   (matrix-*-vector cols mk)) m)))

;;; Same results
;; racket@> (matrix-*-matrix (list (list 1 2 3) (list 4 5 6)) (list (list 1 2) (list 3 4) (list 5 6)))
;; '((22 28) (49 64))
