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

;; Exercise 2.38
(define fold-right accumulate)

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
	result
	(iter (op result (car rest))
	      (cdr rest))))
  (iter initial sequence))

;; (fold-right / 1 (list 1 2 3)) should give 3/2
;; (fold-left / 1 (list 1 2 3)) should give 1/6

;;; property that op should satisfy to guarantee that fold-right and fold-left will produce the same values for
;;; any sequence -- it would have to be associative like the typical algebraic ops

(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) nil sequence))

;; racket@> (reverse (list 1 2 3))
;; '(3 2 1)

(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))

;; racket@> (reverse (list 1 2 3))
;; '(3 2 1)


;;; Nested Mappings
(define (flatmap proc seq)
	(accumulate append nil (map proc seq)))

(define (prime-sum? pair)
	(prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
	(list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
	(map make-pair-sum
		(filter prime-sum?
				(flatmap
					(lambda (i)
						(map (lambda (j) (list i j))
							 (enumerate-interval 1 (- i 1))))
					(enumerate-interval 1 n)))))

;; > (prime-sum-pairs 6)
;; '((2 1 3) (3 2 5) (4 1 5) (4 3 7) (5 2 7) (6 1 7) (6 5 11))

(define (permutations s)
	(if (null? s)
		(list nil)
		(flatmap (lambda (x)
					(map (lambda (p) (cons x p))
						 (permutations (remove x s))))
				 s)))

;; > (permutations (list 1 2 3))
;; '((1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 1 2) (3 2 1))

;; Filter implementations of remove
(define (remove item seq)
	(filter (lambda (x) (not (= x item))) seq))

;; Still works
;; > (permutations (list 1 2 3))
;; '((1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 1 2) (3 2 1))

;;; Exercise 2.40

(define (unique-pairs n)
  (flatmap (lambda (i)
		(map (lambda (j)
		       (list i j)) (enumerate-interval 1 (- i 1)))) (enumerate-interval 1 n)))

(define (prime-sum-pairs n)
  (map make-pair-sum (filter prime-sum? (unique-pairs n))))

;;; Exercise 2.41
(define (triple-sum-eq triple s)
  (= (accumulate + 0 triple) s))

(define (>!=? accum x)
  (let ((y (car accum))
	(z (cadr accum)))
    (list x (and z (> x y) (not (= x y))))))

(define (distinct-and-ordered? seq)
  (cadr (fold-left >!=? '(0 #t) seq)))

(define (make-triples n)
  (flatmap (lambda (i)
	       (flatmap (lambda (j)
		      (map (lambda (k)
			     (list i j k)) (enumerate-interval 1 n)))
		    (enumerate-interval 1 n)))
	   (enumerate-interval 1 n)))

;;; if n > s, we don't need em, so lets just use s
(define (find-distinct-ordered-triples-for-sum n s)
  (filter (lambda (i)
	    (triple-sum-eq i s)) (filter distinct-and-ordered? (make-triples s))))


;;; This seems to do the trick, but is very brute force. I think it should be possible to generate the list
;;; without first  building all tuples
(define (make-triples n)
  (flatmap (lambda (i)
	       (flatmap (lambda (j)
		      (map (lambda (k)
			     (list i j k)) (enumerate-interval (+ j 1) n)))
		    (enumerate-interval (+ i 1) n)))
	   (enumerate-interval 1 n)))

;;; with old make-triples
;; racket@> (time (find-distinct-ordered-triples-for-sum 10000 100))
;; cpu time: 750 real time: 750 gc time: 390

;; racket@> (time (find-distinct-ordered-triples-for-sum 10000 100))
;; cpu time: 190 real time: 191 gc time: 67

;;; Exercise 2.42 - Eight Queens Puzzle
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
	(list empty-board)
	(filter
	 (lambda (positions) (safe? k positions))
	 (flatmap
	  (lambda (rest-of-queens)
	    (map (lambda (new-row)
		   (adjoin-position new-row k rest-of-queens))
		 (enumerate-interval 1 board-size)))
	  (queen-cols (- k 1))))))
  (queen-cols board-size))

;;; What makes a queen safe?
;;;  - not in the same row, column, or diagonal

;;; Positions represented by a list pair
;;; i = columns, j = rows
(define empty-board nil)

(define (make-position i j)
  (list i j))
(define (position-row pos)
  (cadr pos))
(define (position-col pos)
  (car pos))

(define (positions-minus pos1 pos2)
  (make-position
   (- (position-col pos1) (position-col pos2))
   (- (position-row pos1) (position-row pos2))))

(define (position-abs pos)
  (map (lambda (i)
	 (abs i)) pos))

(define (position-is-diag? pos)
  (= (position-row pos) (position-col pos)))

(define (positions-share-row? pos1 pos2)
  (= (position-row pos1) (position-row pos2)))

(define (positions-share-col? pos1 pos2)
  (= (position-col pos1) (position-col pos2)))

(define (safe? column positions)
  (if (= column 1)
      #t
      (let ((posk (car positions))
	    (rest (cdr positions)))
	(accumulate (lambda (pos accum)
		      (and accum (not (or
				       (positions-share-row? posk pos)
				       (positions-share-col? posk pos)
				       (position-is-diag? (position-abs (positions-minus posk pos)))))))
		    #t rest))))

(define (adjoin-position new-row column rest-of-queens)
  (append (list (list column new-row)) rest-of-queens))

;;; When I run this, I do find the solution in Figure 8 in my set
;;; This was a real thinker for me. I probably spent so much time on it (~ 2 hours) because I had to first
;;; understand how the recursion was working. It also took an almost embarassing amount of time for my brain to
;;; understand how to quantify pieces being on a diagonal from each other.Also, it's almost harder to fill in after
;;; the fact than to start from the bottom up.
;;; Solution in Figure 2.8:
 ;; ((8 3) (7 5) (6 8) (5 4) (4 1) (3 7) (2 2) (1 6))

;;; The solutions for a 2x2 and 3x3 board make sense, since 2x2 no matter where you place column 2 they are
;;; in check with the other. In 3x3, you can place the first 2 columns, but column 3 can't be placed. Let's
;;; check our results for a 4x4

;; racket@> (queens 4)
;; '(((4 3) (3 1) (2 4) (1 2)) ((4 2) (3 4) (2 1) (1 3)))

;; +---+---+---+---+
;; |   | x |   |   |
;; +---+---+---+---+
;; +---+---+---+---+
;; |   |   |   | x |   ;; Checks out.
;; +---+---+---+---+
;; +---+---+---+---+
;; | x |   |   |   |
;; +---+---+---+---+
;; +---+---+---+---+
;; |   |   | x |   |
;; +---+---+---+---+

;; +---+---+---+---+
;; |   |   | x |   |
;; +---+---+---+---+
;; +---+---+---+---+
;; | x |   |   |   |  ;; Also checks out.
;; +---+---+---+---+
;; +---+---+---+---+
;; |   |   |   | x |
;; +---+---+---+---+
;; +---+---+---+---+
;; |   | x |   |   |
;; +---+---+---+---+

;;; Exercise 2.43
(define (queens-bad board-size)
  (define (queen-cols k)
    (if (= k 0)
	(list empty-board)
	(filter
	 (lambda (positions) (safe? k positions))
	 (flatmap
	  (lambda (new-row)
	    (map (lambda (rest-of-queens)
		   (adjoin-position new-row k rest-of-queens))
		 (queen-cols (- k 1))))
	  (enumerate-interval 1 board-size)))))
  (queen-cols board-size))

;;; While (queens-bad 8) is running forever, I will try and explain why it takes so long.
;;; During each 'map' under the flatmap's lambda, it is recursing all the way down until k=1
;;; Each recursion, is then running its own 'map', when then recurses down again. So there is a 
;;; lot of recursion going on (to put it midly).

;;; Board Size | queens | queens-bad
;;;     8      |    23  |   31496     |    1369
;;;     9      |   130  |  841858     |    6475  
;;;    10      |   923  |    


;;; Section 2.2.4 - A Picture Language

;; Example right-split 2
;;;(define (right-split painter 2)
;;;	(if (= 2 0)
;;;		painter
;;;		(let ((smaller (
;;;						(if (= 1 0)
;;;							painter
;;;							(let ((smaller (
;;;											(if (= 0 0)
;;;												painter
;;;											)
;;;									(beside painter (below smaller smaller))

;;; Exercise 2.44
(define (up-split painter n)
	(if (= n 0)
		painter
		(let ((smaller (up-split painter (- n 1))))
			(below painter (beside smaller smaller)))))
			
;;; Exercise 2.55
(define (split dir1 dir2)
	(lambda (painter n)
		(if (= n 0)
			painter
			(let ((smaller ((split dir1 dir2) painter (- n 1))))
				(dir1 painter (dir2 smaller smaller))))))

;;; It's a little unclear what is happening with the lambda, so instead
(define (split dir1 dir2)
	(define (splitter painter n)
		(if (= n 0)
			painter
			(let ((smaller (splitter painter (- n 1))))
			  (dir1 painter (dir2 smaller smaller))))))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect (scale-vect (xcor-vect v)
			  (edge1-frame frame))
	      (scale-vect (ycor-vect v)
			  (edge2-frame frame)))))


;;; Exercise 2.46 - Vector Definitions
(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))
(define (ycor-vect v)
  (cdr v))

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2)) (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2)) (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v)) (* s (ycor-vect v))))


;;; Exercise 2.47 - Frame Constructors
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame frame)
  (car frame))
(define (edge1-frame frame)
  (cadr frame))
(define (edge2-frame frame)
  (caddr frame))

;; racket@> (origin-frame (make-frame 1 2 3))
;; 1
;; racket@> (edge1-frame (make-frame 1 2 3))
;; 2
;; racket@> (edge2-frame (make-frame 1 2 3))
;; 3

(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (edge2-frame frame)
  (cddr frame))

;; racket@> (origin-frame (make-frame 1 2 3))
;; 1
;; racket@> (edge1-frame (make-frame 1 2 3))
;; 2
;; racket@> (edge2-frame (make-frame 1 2 3))
;; caddr: contract violation
;;   expected: (cons/c (cons/c any/c pair?) any/c)
;;   given: '(1 2 . 3)
;;   context...:
;;    /usr/share/racket/collects/racket/private/norm-define.rkt:53:83: edge2-frame
;;    /usr/share/racket/collects/racket/private/misc.rkt:87:7
;; racket@> (edge2-frame (make-frame 1 2 3))
;; 3

;;; Exercise 2.48
(define (make-segment v1 v2)
  (cons v1 v2))

(define (start-segment seg)
  (car seg))

(define (end-segment seg)
  (cdr seg))

;;; Exercise 2.49

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
	((frame-coord-map frame) (start-segment segment))
	((frame-coord-map frame) (end-segment segment))))
     segment-list)))

;;; Need to be able to turn the frame into a list of line segments
;;; origin + edge1, origin+edge2, origin+edge2+edge1, origin+edge1+edge2
(let ((tl (make-vect 0 1))
      (tr (make-vect 1 1))
      (bl (make-vect 0 0))
      (br (make-vect 1 0)))
;; a) Draw the outline of the designated frame
  (segments->painter (list
		      (make-segment bl tl)
		      (make-segment tl tr)
		      (make-segment tr br)
		      (make-segment br bl)))
  ;; b) draw an X
  (segments->painter (list
		      (make-segment bl tr)
		      (make-segment br tl)))
  (let ((l (make-vect 0 0.5))
	(t (make-vect 0.5 1))
	(r (make-vect 1 0.5))
	(b (make-vect 0.5 0)))
    (segments->painter (list
			(make-segment l t)
			(make-segment t r)
			(make-segment r b)
			(make-segment b l)))))

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
	(painter
	 (make-frame new-origin
		     (sub-vect (m corner1) new-origin)
		     (sub-vect (m corner2) new-origin)))))))

(define (flip-vert painter)
  (transform-painter painter
		     (make-vect 0.0 1.0)
		     (make-vect 1.0 1.0)
		     (make-vect 0.0 0.0)))

(define (shrink-to-upper-right painter)
  (transform-painter painter
		     (make-vect 0.5 0.5)
		     (make-vect 1.0 0.5)
		     (make-vect 0.5 1.0)))

(define (rotate90 painter)
  (transform-painter painter
		     (make-vect 1.0 1.0)
		     (make-vect 1.0 1.0)
		     (make-vect 1.0 0.0)))

;;; Exercise 2.50

(define (flip-horiz painter)
  (transform-painter painter
		     (make-vect 1.0 0.0)
		     (make-vect 0.0 0.0)
		     (make-vect 1.0 1.0)))

(define (rotate180 painter)
  (rotate90 (rotate90 painter)))

(define (rotate270 painter)
  (rotate90 (rotate180 painter)))

(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-below
	   (transform-painter painter1
			      (make-vect 0.0 0.0)
			      (make-vect 1.0 0.0)
			      split-point))
	  (paint-above
	   (transform-painter painter2
			      split-point
			      (make-vect 1.0 0.5)
			      (make-vect 0.0 1.0))))
      (lambda (frame)
	(paint-below frame)
	(paint-above frame)))))

(define (below painter1 painter2)
  (rotate90 (beside (rotate270 painter1) (rotate270 painter2))))

;;; In this book the "closure property" is in the abstract algebra sense. That if an operation on members of a set produces members of the same set. The operation is "closed"

;;; Skipping Exercise 2.52


