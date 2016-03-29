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

;; Exercise 2.56
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
	((exponentiation? exp)
	 (make-product
	  (make-product (exponent exp)
			(make-exponentiation (base exp)
					     (- (exponent exp) 1)))
	  (deriv (base exp) var)))
	(else
	 (error "unknown expression type -- DERIVE" exp))))

(define (make-exponentiation base exponent)
  (cond ((= exponent 0) 1)
	((= exponent 1) base)
	(else (list '** base exponent))))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base x)
  (cadr x))
(define (exponent x)
  (caddr x))


;;; Exercise 2.57
(define (make-sum . as)
  (let ((nsum (foldr + 0 (filter (lambda (x) (number? x)) as)))
	(syms (filter (lambda (x) (not (number? x))) as)))
    (if (null? syms)
	nsum
	(cond ((null? syms) nsum)
	      ((and (=number? nsum 0) (not (pair? (cdr syms)))) (car syms))
	      ((and (=number? nsum 0) (pair? (cdr syms))) (cons '+ syms))
	      (else
	       (cons '+ (cons nsum syms)))))))

(define (make-product . ms)
  (let ((nprod (foldr * 1 (filter (lambda (x) (number? x)) ms)))
	(syms (filter (lambda (x) (not (number? x))) ms)))
    (if (null? syms)
	nprod
	(cond ((=number? nprod 0) 0)
	      ((and (=number? nprod 1) (not (pair? (cdr syms)))) (car syms))
	      ((and (=number? nprod 1) (pair? (cdr syms))) (cons '* syms))
	      (else (cons '* (cons nprod syms)))))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))
(define (augend s)
  (apply make-sum (cddr s)))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))
(define (multiplicand p) (apply make-product (cddr p)))

;;; This undoubtedly works.
;; racket@> (deriv '(* x y (+ x 3)) 'x)
;; '(+ (* x y) (* y (+ x 3)))

;;; But there is a more elgant way using our original make-sum/make-product
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

(define (augend s)
  (foldr make-sum 0 (cddr s)))

(define (multiplicand s)
  (foldr make-product 1 (cddr s)))

;;; I find the above a much clearer way to make to extend it to multiple args
;; racket@> (deriv '(* x y (+ x 3)) 'x)
;; '(+ (* x y) (* y (+ x 3)))

;;; Exercise 2.58
;;; a)
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
	((=number? a2 0) a1)
	((and (number? a1) (number? a2) (+ a1 a2)))
	(else (list a1 '+ a2))))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
	((=number? m1 1) m2)
	((=number? m2 1) m1)
	((and (number? m1) (number? m2)) (* m1 m2))
	(else (list m1 '* m2))))

(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))

(define (addend s) (car s))
(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier p) (car p))
(define (multiplicand p) (caddr p))

;; racket@> (deriv '(x + (3 * (y + 2))) 'x)
;; 1
;; racket@> (deriv '(x * y) 'x)
;; 'y
;; racket@> (deriv '((x * y) * (x + 3)) 'x)
;; '((x * y) + (y * (x + 3)))

;;; TODO : Part B
(define exec-steps 0)
;;; Sets As unordered Lists
(define (element-of-set? x set)
  (set! exec-steps (+ exec-steps 1))
  (cond ((null? set) false)
	((equal? x (car set)) true)
	(else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
	((element-of-set? (car set1) set2)
	 (cons (car set1)
	       (intersection-set (cdr set1) set2)))
	(else (intersection-set (cdr set1) set2))))

(define (intersection-set-iter set1 set2)
  (define (iter set1 set2 inter)
    (cond ((or (null? set1) (null? set2)) inter)
	  ((element-of-set? (car set1) set2)
	   (iter (cdr set1) set2 (cons (car set1) inter)))
	  (else
	   (iter (cdr set1) set2 inter))))
  (iter set1 set2 '()))

;;; Exercise 2.59
(define (union-set set1 set2)
  (cond ((null? set1) set2)
	((null? set2) set1)
	((not (element-of-set? (car set1) set2))
	 (cons (car set1) (union-set (cdr set1) set2)))
	(else (union-set (cdr set1) set2))))

(define (union-set-iter set1 set2)
  (define (iter set1 set2 union)
    (cond ((null? set1) set2)
	  ((null? set2) set1)
	  ((element-of-set? (car set1) set2)
	   (iter (cdr set1) set2 union))
	  (else
	   (iter (cdr set1) set2 (cons (car set1) union)))))
  (iter set1 set2 '()))

;; racket@> (union-set '() '())
;; '()
;; racket@> (union-set '() '(1 2 3 4 5 6))
;; '(1 2 3 4 5 6)
;; racket@> (union-set '(1 2 3 4 9 6 7) '())
;; '(1 2 3 4 9 6 7)
;; racket@> (union-set '(1 2 3 4 9 6 7) '(1 2 3 4 5 6))
;; '(9 7 1 2 3 4 5 6)
;; racket@> (union-set '(1 2 3 4) '(1 2 3 4 5 6))
;; '(1 2 3 4 5 6)
;; racket@> (union-set '(1 2 3 4) '(1 2 3 4))
;; '(1 2 3 4)

;;; functionally the same as
(define (union-set set1 set2)
  (cond ((null? set1) set2)
	((null? set2) set1)
	((element-of-set? (car set1) set2)
	 (union-set (cdr set1) set2))
	(else (cons (car set1)
		    (union-set (cdr set1) set2)))))

;;; Exercise 2.60
;;; Allow duplicates
;;; Initial thoughts : This will be hard to gauge. Since duplicates are allowed, it is possible that a value
;;; That is buried way at the back of the list is present towards the head. In this sense, if duplicates are likely
;;; and spread out, it is possible that some steps might be saved. Otherwise, it is just increasing the size of the
;;; problem (n).
(define (element-of-set-dup? x set)
  (cond ((null? set) false)
	((equal? x (car set)) true)
	(else (element-of-set? x (cdr set)))))

(define (adjoin-set-dup x set)
  (cons x set))

;;; Union is easy if dups are present, we can just cons them together. This operation will only depend on
;;; the number of elements (n) in set1.
(define (union-set-dup set1 set2)
  (cond ((null? set1) set2)
	((null? set2) set1)
	(else
	 (cons (car set1) (union-set (cdr set1) set2)))))

;;; I think this is where the performance penalty will come. We still have to check every single element
;;; set1 vs set2. So if n is large and there are lots of duplicates, then we have increased our problem
;;; size greatly. Duplicates in set1 will only show up in the union set. No duplicates from set2 will
;;; transfer.
(define (intersection-set-dup set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
	((element-of-set? (car set1) set2)
	 (cons (car set1)
	       (intersection-set (cdr set1) set2)))
	(else
	 (intersection-set (cdr set1) set2))))

(define (build-set n m)
  (define (iter i l)
    (if (< i n)
	l
	(iter (- i 1) (cons i l))))
  (iter m '()))
;;; Investigations into efficiency



;;; Ordered Variations

(define (element-of-set-ordered? x set)
  (cond ((null? set) false)
	((= x (car set)) true)
	((< x (car set)) false)
	(else (element-of-set-ordered? x (cdr set)))))

(define (intersection-set-ordered set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
	(cond ((= x1 x2)
	       (cons x1
		     (intersection-set-ordered (cdr set1)
				       (cdr set2))))
	      ((< x1 x2)
	       (intersection-set-ordered (cdr set1) set2))
	      ((< x2 x1)
	       (intersection-set-ordered set1 (cdr set2)))))))

;;; Exercise 2.61
(define (adjoin-set-ordered x set)
  (cond ((null? set) (cons x set))
	((> x (car set))
	 (cons (car set) (adjoin-set-ordered x (cdr set))))
	((= x (car set))
	 set)
	((< x (car set))
	 (cons x set))))

;; racket@> (adjoin-set 7 (adjoin-set 4 (adjoin-set 11 '(1 2 3 5 6 9 10))))
;; '(1 2 3 4 5 6 7 9 10 11)

;;; Exercise 2.62
;;; O(n) union-set
(define (union-set-ordered set1 set2)
  (cond ((null? set1) set2)
	((null? set2) set1)
	(else
	 (let ((x1 (car set1)) (x2 (car set2)))
	   (cond ((< x1 x2)
		  (cons x1 (union-set-ordered (cdr set1) set2)))
		 ((= x1 x2)
		  (union-set-ordered (cdr set1) set2))
		 ((> x1 x2)
		  (cons x2 (union-set-ordered (cdr set2) set1))))))))


;; racket@> (union-set-ordered '(4 5 6 10 11 12) '(1 2 3 4 5 7 8 9))
;; '(1 2 3 4 5 6 7 8 9 10 11 12)

;;; Sets as binary trees

(define (entry tree)
  (car tree))

(define (left-branch tree)
  (cadr tree))

(define (right-branch tree)
  (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set-tree? x set)
  (cond ((null? set) false)
	((= x (entry set)) true)
	((< x (entry set))
	 (element-of-set-tree? x (left-branch set)))
	((> x (entry set))
	 (element-of-set-tree? x (right-brach set)))))

(define (adjoin-set-tree x set)
  (cond ((null? set) (make-tree x '() '()))
	((= x (entry set)) set)
	((< x (entry set))
	 (make-tree (entry set)
		    (adjoin-set-tree x (left-branch set))
		    (right-branch set)))
	((> x (entry set))
	 (make-tree (entry set)
		    (left-branch set)
		    (adjoin-set-tree x (right-branch set))))))



;;; Exercise 2.63
;;; Initial thoughts: I expect tree->list-2 to be more performant.
;;; 1. list-1 uses append and pure recursion
;;; 2. list-2 uses a mixture of iteration and recursion
;;; Both lists should produce the same list
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
	      (cons (entry tree)
		    (tree->list-1 (right-branch tree))))))


(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
	result-list
	(copy-to-list (left-branch tree)
		      (cons (entry tree)
			    (copy-to-list (right-branch tree)
					  result-list)))))
  (copy-to-list tree '()))


;;; Balanced tree produces identical list
;; racket@> (tree->list-1 tree2-16a)
;; '(1 3 5 7 9 11)
;; racket@> (tree->list-2 tree2-16a)
;; '(1 3 5 7 9 11
;; racket@> (tree->list-1 tree2-16b)
;; '(1 3 5 7 9 11)
;; racket@> (tree->list-2 tree2-16b)
;; '(1 3 5 7 9 11)
;; racket@> (tree->list-2 tree2-16c)
;; '(1 3 5 7 9 11)
;; racket@> (tree->list-1 tree2-16c)
;; '(1 3 5 7 9 11)

;;; Exercise 2.64
;; This procedure calculates the middle of an ordered list (quotient (- n 1) 2) then builds the left subtree on that
;; middle point by recusring through the building each subtree to the left of the middle. This subtree is then passed
;; back up to the entry point with the remaining elements of the list. It then recurses down and builds the right
;; subtree. At the end, the tree is built by building a list with the middle point, the left subtree and the right
;; subtree. Since each half is built independently, the function is only called 2 * n/2 times, so the procedure
;; is O (n).

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
	(let ((left-result (partial-tree elts left-size)))
	  (let ((left-tree (car left-result))
		(non-left-elts (cdr left-result))
		(right-size (- n (+ left-size 1))))
	    (let ((this-entry (car non-left-elts))
		  (right-result (partial-tree (cdr non-left-elts)
					      right-size)))
	      (let ((right-tree (car right-result))
		    (remaining-elts (cdr right-result)))
		(cons (make-tree this-entry left-tree right-tree)
		      remaining-elts))))))))


;;; Exercise 2.65 - O(n) Union set and Intersection Set
;;; using results from 2.63 - 2.64
;;; these have the advantage of producing a balanced-ish tree as a result since they use partial-tree.
;;; my solution for using pure trees below does not.
(define (union-set-tree set1 set2)
  (list->tree (union-set-ordered (tree->list-2 set1) (tree->list-2 set2))))

(define (intersection-set-tree set1 set2)
  (list->tree (intersection-set-ordered (tree->list-2 set1) (tree->list-2 set2))))

;; racket@> (union-set-tree '(2 (1 () ()) (3 () ())) '(5 (4 () ()) (6 () ())))
;; '(3 (1 () (2 () ())) (5 (4 () ()) (6 () ())))
;; racket@> (intersection-set-tree '(2 (1 () ()) (3 () ())) '(5 (4 (2 (1 () ()) (3 () ())) ()) (6 () ())))
;; '(2 (1 () ()) (3 () ()))

;; racket@> (union-set-tree (list->tree '(1 3 5 7 9 11)) (list->tree '(2 4 6 8 10 11 12)))
;; '(6
;;   (3 (1 () (2 () ())) (4 () (5 () ())))
;;   (9 (7 () (8 () ())) (11 (10 () ()) (12 () ()))))
;; racket@> (tree->list-2 (union-set-tree (list->tree '(1 3 5 7 9 11)) (list->tree '(2 4 6 8 10 11 12))))
;; '(1 2 3 4 5 6 7 8 9 10 11 12)

;;; Using tree representations
(define (union-set-tree-2 set1 set2)
    (cond ((null? set1) set2)
	  ((null? set2) set1)
	  (else
	   (let ((x1 (entry set1)) (x2 (entry set2)))
	     (cond ((< x1 x2)
		    (make-tree x2
			       (union-set-tree-2 set1 (left-branch set2))
			       (right-branch set2)))
		   ((> x1 x2)
		    (make-tree x2
			       (left-branch set2)
			       (union-set-tree-2 set1 (right-branch set2))))
		   ((= x1 x2)
		    (make-tree x1
			       (union-set-tree-2 (left-branch set1)
					       (left-branch set2))
			       (union-set-tree-2 (right-branch set1)
					       (right-branch set2)))))))))

(define (intersection-set-tree-2 set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (entry set1)) (x2 (entry set2)))
	(cond ((< x1 x2)
	       (intersection-set-tree-2 set1 (left-branch set2)))
	      ((> x1 x2)
	       (intersection-set-tree-2 set1 (right-branch set2)))
	      ((= x1 x2)
	       (make-tree x1
			  (intersection-set-tree-2 (left-branch set1) (left-branch set2))
			  (intersection-set-tree-2 (right-branch set1) (right-branch set2))))))))


;; racket@> (union-set-tree '(2 (1 () ()) (3 () ())) '(5 (4 () ()) (6 () ())))
;; '(5 (4 (2 (1 () ()) (3 () ())) ()) (6 () ()))
;; racket@> (intersection-set-tree '(2 (1 () ()) (3 () ())) '(5 (4 (2 (1 () ()) (3 () ())) ()) (6 () ())))
;; '(2 (1 () ()) (3 () ()))

;; racket@> (union-set-tree-2 (list->tree '(1 3 5 7 9 11)) (list->tree '(2 4 6 8 10 11 12)))
;; '(8
;;   (4 (2 () ()) (6 (5 (1 () (3 () ())) (9 (7 () ()) (11 () ()))) ()))
;;   (11 (10 () ()) (12 () ())))
;; racket@> (tree->list-2 (union-set-tree-2 (list->tree '(1 3 5 7 9 11)) (list->tree '(2 4 6 8 10 11 12))))
;; '(2 4 1 3 5 7 9 11 6 8 10 11 12)


;;; Exercise 2.66
(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
	((= given-key (key (entry set-of-records)))
	 (entry set-of-records))
	((< given-key (key (entry set-of-records)))
	 (lookup given-key (left-branch set-of-records)))
	(else
	 (lookup given-key (right-branch set-of-records)))))

;;; Huffman Trees
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
	right
	(append (symbols left) (symbols right))
	(+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))


(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
	'()
	(let ((next-branch
	       (choose-branch (car bits) current-branch)))
	  (if (leaf? next-branch)
	      (cons (symbol-leaf next-branch)
		    (decode-1 (cdr bits) tree))
	      (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
	((= bit 1) (right-branch branch))
	(else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
	((< (weight x) (weight (car set))) (cons x set))
	(else (cons (car set)
		    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
	(adjoin-set (make-leaf (car pair)
			       (cadr pair))
		    (make-leaf-set (cdr pairs))))))

;;; Exercise 2.67
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
		  (make-code-tree
		   (make-leaf 'B 2)
		   (make-code-tree (make-leaf 'D 1)
				   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

;; racket@> (decode sample-message sample-tree)
;; '(A D A B B C A)

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
	      (encode (cdr message) tree))))


(define (encode-symbol symbol tree)
  (cond ((leaf? tree) '())
	((found-symbol? symbol (left-branch tree))
	 (cons 0 (encode-symbol symbol (left-branch tree))))
	((found-symbol? symbol (right-branch tree))
	 (cons 1 (encode-symbol symbol (right-branch tree))))
	(else
	 (error "Unable to find symbol -- ENCODE-SYMBOL" symbol))))

(define (found-symbol? symbol branch)
  (if (leaf? branch)
      (eq? symbol (symbol-leaf branch))
      (element-of-set? symbol (symbols branch))))

;; racket@> (encode (decode sample-message sample-tree) sample-tree)
;; '(0 1 1 0 0 1 0 1 0 1 1 1 0)
;; racket@> sample-message
;; '(0 1 1 0 0 1 0 1 0 1 1 1 0)
;; racket@>

;;; After some logic simplification to found-symbol?

;; racket@> sample-message
;; '(0 1 1 0 0 1 0 1 0 1 1 1 0)
;; racket@> (encode (decode sample-message sample-tree) sample-tree)
;; '(0 1 1 0 0 1 0 1 0 1 1 1 0)

;;; still works.

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

;;; Exercise 2.69
;;; make-leaf-set uses adjoin-set to create an ordered list
;;; the first time we use it, we are guaranteed the first two elements will be the smallest, but
;;; what about after our first merge? Might be safer to adjoin the sum onto the cddr.
(define (successive-merge leaf-set)
  (if (null? (cdr leaf-set))
      (car leaf-set)
      (successive-merge (adjoin-set (make-code-tree (car leaf-set) (cadr leaf-set))
				    (cddr leaf-set)))))


;;; Exercise 2.70
(define ex-2.70-tree (generate-huffman-tree '((NA 16) (SHA 3) (YIP 9) (WAH 1) (JOB 2) (GET 2) (BOOM 1) (A 2))))

;; racket@> (length (encode ex-2.70-msgs ex-2.70-tree))
;; 84 bits are needed

;;; If it was a fixed length encoding we would need 3 bits per symbol, we used 36 symbols, so 108 bits total. We saved 24 bits.

