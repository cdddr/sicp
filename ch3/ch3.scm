;;; Section 3.1.1 - Local State Variables
;;; As the example below shows, combining set! with local variables is the general programming technique
;;; that will be used for constructing computational objects with local state.
(define withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
	  (begin (set! balance (- balance amount))
		 balance)
	  "Insufficient funds"))))

;;; As soon as assignment is introdued, the substitution model of evaluation is no longer adequate.
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
	(begin (set! balance (- balance amount))
	       balance)
	"Insufficient Funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
	  ((eq? m 'deposit) deposit)
	  (else (error "Unknown request -- MAKE-ACCOUNT" m))))
  dispatch)

;;; Exercise 3.1
(define (make-accumulator sum)
  (lambda (x)
    (set! sum (+ x sum))
    sum))

;; racket@> (define A (make-accumulator 5))
;; racket@> (A 50)
;; 55
;; racket@> (A 100)
;; 155
;; racket@> (A 2)
;; 157

;;; Exercise 3.2
(define (make-monitored proc)
  (let ((steps 0))
    (define (run . args)
      (set! steps (+ steps 1))
      (apply proc args))
    (define (dispatch x)
      (cond ((eq? x 'how-many-calls?) steps)
	    ((eq? x 'reset-count)
	     (set! steps 0))
	    (else
	     (run x))))
    dispatch))

;; racket@> (define s (make-monitored sqrt))
;; racket@> (s 24)
;; 4.898979485566356
;; racket@> (s 'how-many-calls?)
;; 1
;; racket@> (s 81)
;; 9
;; racket@> (s 'how-many-calls?)
;; 2
;; racket@> (s 'reset-count)
;; racket@> (s 'how-many-calls?)
;; 0

;;; Exercise 3.3 - 3.4
(define (make-account balance password)
  (define badpass (make-accumulator 0))
  (define (withdraw amount)
    (if (>= balance amount)
	(begin (set! balance (- balance amount))
	       balance)
	"Insufficient Funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (call-the-cops x)
    "The police have been notified")
  (define (dispatch pass m)
    (cond ((not (eq? pass password))
	   (if (> (badpass 1) 7)
	       call-the-cops
	       (lambda (x) "Incorrect Password.")))
	  ((eq? m 'withdraw) withdraw)
	  ((eq? m 'deposit) deposit)
	  (else (error "Unknown request -- MAKE-ACCOUNT" m))))
  dispatch)

;; racket@> ((acc 'not-so-secret-password 'withdraw) 40)
;; "Incorrect Password."
;; racket@> ((acc 'super-secret-password 'withdraw) 40)
;; 60

;; racket@> ((acc 'not-so-secret-password 'withdraw) 40)
;; "Incorrect Password."
;; racket@> ((acc 'not-so-secret-password 'withdraw) 40)
;; "Incorrect Password."
;; racket@> ((acc 'not-so-secret-password 'withdraw) 40)
;; "Incorrect Password."
;; racket@> ((acc 'not-so-secret-password 'withdraw) 40)
;; "Incorrect Password."
;; racket@> ((acc 'not-so-secret-password 'withdraw) 40)
;; "Incorrect Password."
;; racket@> ((acc 'not-so-secret-password 'withdraw) 40)
;; "Incorrect Password."
;; racket@> ((acc 'not-so-secret-password 'withdraw) 40)
;; "Incorrect Password."
;; racket@> ((acc 'not-so-secret-password 'withdraw) 40)
;; "The police have been notified"

;;; Section 3.1.2 - The benefits of introducing assignment
;;; Monte Carlo method - choosing sample experiments at random from a large set and making deductions based on
;;; the probabilities estimated from tabulating the results of those experiments.

;;; Exercise - 3.5
;;; Tweaked the random-in-range so that a random decimal is given. This seems to produce much better results.
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (+ (random range) (random)))))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
	   (/ trials-passed trials))
	  ((experiment)
	   (iter (- trials-remaining 1) (+ trials-passed 1)))
	  (else
	   (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (estimate-integral P x1 y1 x2 y2 trials)
  (exact->inexact (* (region-area x1 y1 x2 y2)
		     (monte-carlo trials
				  (lambda ()
				    (region-test P x1 y1 x2 y2))))))
(define (region-area x1 y1 x2 y2)
  (* (- x2 x1) (- y2 y1)))

(define (region-test P x1 y1 x2 y2)
  (let ((x (random-in-range x1 x2))
	(y (random-in-range y1 y2)))
    (P x y)))

(define (in-unit-circle? x y)
  (<= (+ (expt x 2) (expt y 2)) 1))

;; racket@> (estimate-integral in-unit-circle? -2 -2 2 2 1000000)
;; 3.1316
;; racket@> (estimate-integral in-unit-circle? -2 -2 2 2 10000000)
;; 3.1428032
;; racket@> (estimate-integral in-unit-circle? -1 -1 1 1 1000000)
;; 3.1433
;; racket@> (estimate-integral in-unit-circle? -1 -1 1 1 10000000)
;; 3.14046
;; racket@> (estimate-integral in-unit-circle? -1 -1 1 1 10000)
;; 3.1176
;; racket@> (estimate-integral in-unit-circle? -1 -1 1 1 1000)
;; 3.124
;; racket@> (estimate-integral in-unit-circle? -1 -1 1 1 100)
;; 3.0

;;; Exercise 3.6
(define (rand-update x)
  (+ x 2))
(define rand
  (let ((x 0))
    (lambda (action)
      (define worker
	(lambda init
	  (if (null? init)
	      (begin
		(set! x (rand-update x))
		x)
	      (begin
		(set! x (car init))))))
      (cond ((eq? action 'generate)
	     (worker))
	    ((eq? action 'reset)
	     (lambda (i)
	       (worker i)))
	    (else
	     (error "unknown method chosen -- RAND" action))))))

;; racket@> ((rand 'reset) 1)
;; racket@> (rand 'generate)
;; 3
;; racket@> (rand 'generate)
;; 5
;; racket@> (rand 'generate)
;; 7
;; racket@> (rand 'generate)
;; 9
;; racket@> (rand 'generate)
;; 11
;; racket@> ((rand 'reset) 10)
;; racket@> (rand 'generate)
;; 12
;; racket@> (rand 'generate)
;; 14
;; racket@> (rand 'generate)
;; 16
;; racket@> (rand 'generate)
;; 18

;;; A language that supports "equals can be substituted for equals" in an expression without changing the value of
;; expression is said to be referentially transparent. (set! violates referential transparency).
;; What is sameness? What is does it mean for an object to have changed?
;; Cannot determine "change" without some a priori notion of "sameness," and we cannot determine sameness without
;; observing the effects of change.

;; So long as we never modify data objects, we can regard a compound data object to be precisely the totality of its pieces.

;; Programming that makes extensive use of assignment : imperative programming.

;; Exercise 3.7 - Joint Accounts
(define (make-account balance password)
  (let ((badpass (make-accumulator 0))
	(passlist (list password)))
    (define (withdraw amount)
      (if (>= balance amount)
	  (begin (set! balance (- balance amount))
		 balance)
	  "Insufficient Funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (call-the-cops x)
      "The police have been notified")
    (define (add-password npw)
      (set! passlist (cons npw passlist)))
    (define (dispatch pass m)
      (cond ((null? (filter (lambda (pw) (eq? pw pass)) passlist))
	     (if (> (badpass 1) 7)
		 call-the-cops
		 (lambda (x) "Incorrect Password.")))
	    ((eq? m 'withdraw) withdraw)
	    ((eq? m 'deposit) deposit)
	    ((eq? m 'add-password) add-password)
	    (else (error "Unknown request -- MAKE-ACCOUNT" m))))

    dispatch))


(define (make-joint acct opass npass)
  ((acct opass 'add-password) npass)
  acct)

;; ;;> (define paul-acc (make-joint peter-acc 'open-sesame 'rosebud))
;; ;;> ((paul-acc 'rosebud 'withdraw) 10)
;; ;;90
;; ;;> ((peter-acc 'open-sesame 'deposit) 100)
;; ;;190
;; ;;> ((paul-acc 'rosebud 'withdraw) 200)
;; ;;"Insufficient Funds"
;; ;;> ((paul-acc 'rosebud 'withdraw) 189)
;; ;;1

;; ;; Exercise 3.8 - Importance of evaluation order now that we introduced assignment
;; ;; A simple procedure such that (+ (f 0) (f 1)) will return 0 if arguments are evaluated left-right, and 1 if right->left
(define f
  (let ((x 0)
        (c 0))
    (lambda (i)
      (set! x (* c (+ x 1)))
      (set! c i)
      x))) 

;; ;;> (+ (f 0) (f 1))
;; ;;0
;; ;;> (load "/home/_2sexp/learn/sicp/ch3.scm")
;; ;;> (+ (f 1) (f 0))
;; ;;1

;; ;; Once assignment is introduced, a variable is no longer just a name for a value. Instead, it designates a "place" in which values can
;; ;; be stored. These places will be called "environments". An environment is a sequence of frames. Each frame is a table of bindings which
;; ;; associate variable names with their corresponding values. Each frame will also have a pointer to its enclosing environment. Frames will
;; ;; be considered to be global. The value of a variable with respect to an environment will be given by the binding of the variable in the
;; ;; first frame of the environment that contains a binding for that variable. If no frame specifies a binding for the variable, the variable
;; ;; is said to be unbound in the environment.

;; ;; An expression acquires meaning only with respect to some environment in which it is evaluated.

;; ;; In general, define creates definitions by adding bindings to frames.

;; ;; To apply a procedure:
;; ;;  1. Create a new environment containing a frame that binds the parameters to the values of the arguments.
;; ;;     a) The enclosing environment for this environment is the environment pointed to by the procedure definition.
;; ;;  2. Within this new environment, evaluate the procedure body.

;; ;; A procedure is created by evaluating a lambda expression relative to a given environment.

;; ;; Exercise 3.9 - 3.11 - In Notebook.

;; ;; Section 3.3 - Modeling with Mutable Data

;; ;; Exercise 3.12 - 3.13 - In Notebook.
(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)
(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

;; ;; Exercise 3.14 - From my notebook, this should destructively reverse the input x.
(define (mystery x)
  (define (loop x y)
    (if (null? x)
	y
	(let ((temp (cdr x)))
	  (set-cdr! x y)
	  (loop temp x))))
  (loop x '()))

;; ;;#;11> (define v (list 'a 'b 'c 'd))
;; ;;#;12> v
;; ;;(a b c d)
;; ;;#;13> (define w (mystery v))
;; ;;#;14> v
;; ;;(a)
;; ;;#;15> w
;; ;;(d c b a)

;; ;; Exercise 3.15 - In Notebook
;; ;;#;1> (define x (list 'a 'b))
;; ;;#;2> x
;; ;;(a b)
;; ;;#;3> (define z1 (cons x x))
;; ;;#;4> z1
;; ;;((a b) a b)
;; ;;#;5> (car z1)
;; ;;(a b)
;; ;;#;6> (define (set-to-wow! x)
;; ;;	   (set-car! (car x) 'wow))
;; ;;#;7> (set-to-wow! z1)
;; ;;#;8> z1
;; ;;((wow b) wow b)

;; ;; Exercise 3.16 - counting pairs in a list
(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
	 (count-pairs (cdr x))
	 1)))

;; ;; Representation that returns 3
;; ;;#;14> (count-pairs (list 'a 'b 'c))
;; ;;3

;; ;;Representation that returns 4
;; ;;((a . b) (a . b) . b)


;; ;;Representation that returns 7
;; ;;(((a . b) a . b) (a . b) a . b)

;; ;;Never returns
;; ;;#;18> (define z1 (list 'a 'b 'c))
;; ;;#;19> (make-cycle z1)


;; ;; Exercise 3.17 - Counting pairs
(define (count-pairs x)
  (let ((known-pairs '()))
    (define (counter x)
      (if (or (not (pair? x)) (memq x known-pairs))
      	  0
          (begin
      	    (set! known-pairs (cons x known-pairs))
      	    (+ (counter (car x))
      	       (counter (cdr x))
      	       1))))
    (counter x)))

;; ;; Exercise 3.18 - Detecting a cyclic list
(define (detect-cycle lst)
  (let ((first (car lst)))
    (set-car! lst first)
    (define (detect lst)
      (cond ((null? (cdr lst)) #f)
      	    ((pair? (car lst)) (detect-cycle (car lst)))
            ((eq? (car (cdr lst)) first) #t)
            (else
	     (detect (cdr lst)))))
    (detect lst)))
;; ;;#;44> (detect-cycle (list 'a 'b 'c))
;; ;;#f
;; ;;#;45> (detect-cycle z1)
;; ;;#t
;; ;;#;56> (define z2 (list 'a 'b 'c))
;; ;;#;57> z2
;; ;;(a b c)
;; ;;#;58> (define z3 (cons z2 '(a b c)))
;; ;;#;59> z3
;; ;;((a b c) a b c)
;; ;;#;60> (set-cdr! (cddr z2) z2)
;; ;;#;61> z2
;; ;;(a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a b c a...
;; ;;#;81> (detect-cycle z2)
;; ;;#t
;; ;;#;82> (detect-cycle z3)
;; ;;#t

;; ;; Exercise 3.19 - I don't think I need to do this one. My solution to 3.18 should use constant space.
;; ;; I realized the above only works if it cycles back to the first element, but I'm moving on.
;; ;;#;90> (define z1 '(a b c))
;; ;;#;91> (set-cdr! (cddr z1) (cdr z1))
;; ;;#;92> z1
;; ;;(a b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b c b...
;; ;;#;93> (detect-cycle z1)
;; ;;
;; ;;*** user interrupt ***

;; ;; Exercise 3.20 - In Notebook.

;; ;; Section 3.3.2 - Representing Queues
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))
(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))


(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue) 
	 (error "DELETE! called with an empty queue" queue))
	(else
	 (set-front-ptr! queue (cdr (front-ptr queue)))
	 queue)))


;; ;;Exercise - 3.21
(define (print-queue queue)
  (front-ptr queue))

;; ;;Exercise - 3.22 - Queues - Procedural Representation
(define (make-queue)
  (let ((front-ptr '())
	(rear-ptr '()))
    (define (empty-queue?)
      (null? front-ptr))
    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
	(cond ((empty-queue?)
	       (set! front-ptr new-pair)
	       (set! rear-ptr new-pair))
	      (else
	       (set-cdr! rear-ptr new-pair)
	       (set! rear-ptr new-pair)))
	front-ptr))
    (define (delete-queue!)
      (cond ((empty-queue?)
	     (error "DELETE! called with an empty queue"))
	    (else
	     (set! front-ptr (cdr front-ptr))))
      front-ptr)
    (define (front-queue)
      (if (empty-queue?)
	  (error "FRONT-QUEUE called with an empty queue")
	  (car front-ptr)))
    (define (dispatch m)
      (cond ((eq? m 'empty-queue?) (empty-queue?))
	    ((eq? m 'insert-queue!) insert-queue!)
	    ((eq? m 'delete-queue!) (delete-queue!))
	    ((eq? m 'front-queue) (front-queue))
	    (else
	     (error "Bad dispatch in MAKE-QUEUE" m))))
    dispatch))

(define (empty-queue? z)
  (z 'empty-queue?))

(define (insert-queue! z item)
  ((z 'insert-queue!) item))
(define (delete-queue! z)
  (z 'delete-queue!))
(define (front-queue z)
  (z 'front-queue))

;;#;263> (insert-queue! q1 'a)
;;(a)
;;#;264> (delete-queue! q1)
;;()
;;#;265> (insert-queue! q1 'b)
;;(b)
;;#;266> (insert-queue! q1 'c)
;;(b c)
;;#;267> (delete-queue! q1)
;;(c)
;;#;268> (delete-queue! q1)
;;()

;;; Exercise 3.23 - Deques
;; Should be able to re-use alot of the queue framework, except items will be added to the front and rear
;; of the list. This was midly wrong. The guts are mostly the same, but the internal representation was a little tricky.
;; Basically the equivalent of a doubly linked list. The internal structure used is (pointer-i-1 i pointer i+1) where i is the 
;; place in the list.
(define (make-deque)
  (cons '() '()))
(define (empty-deque? deque)
  (or (null? (front-ptr deque))
      (null? (rear-ptr deque))))
(define (front-ptr deque) (car deque))
(define (rear-ptr deque) (cdr deque))
(define (front-deque deque)
  (cadr (front-ptr deque)))
(define (rear-deque deque)
  (cadr (rear-ptr deque)))
(define (set-front-ptr! deque item)
  (set-car! deque item))
(define (set-rear-ptr! deque item)
  (set-cdr! deque item))
(define (front-insert-deque! deque item)
  (let ((new-item (cons '() (cons item '()))))
    (cond ((empty-deque? deque)
	   (set-front-ptr! deque new-item)
	   (set-rear-ptr! deque new-item))
	  (else
	   (set-car! (front-ptr deque) new-item)
	   (set-cdr! (cdr new-item) (front-ptr deque))
	   (set-front-ptr! deque new-item)))))
(define (rear-insert-deque! deque item)
  (let ((new-item (cons '() (cons item '()))))
    (cond ((empty-deque? deque)
	   (set-front-ptr! deque new-item)
	   (set-rear-ptr! deque new-item))
	  (else
	   (set-car! new-item (rear-ptr deque))
	   (set-cdr! (cdr (rear-ptr deque)) new-item)
	   (set-rear-ptr! deque new-item)))))
(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
	 (error "FRONT-INSERT-DELETE called on an empty deque."))
	(else
	 (set-front-ptr! deque (cddr (front-ptr deque))))))
(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
	 (error "REAR-INSERT-DELETE called on an empty deque"))
	(else
	 (set-rear-ptr! deque (car (rear-ptr deque))))))
(define (print-deque deque)
  (define (iter item res)
    (cond ((empty-deque? deque) res)
	  ((null? (car item)) (cons (cadr item) res))
	  (else
	   (iter (car item) (cons (cadr item) res)))))
  (iter (rear-ptr deque) '()))

(define dq1 (make-deque))

;; Section 3.3.3 - Tables
(define (lookup key table)
  (let ((record (assoc1 key (cdr table))))
    (if record
	(cdr record)
	#f)))
(define (assoc key records)
  (cond ((null? records) #f)
	((equal? key (caar records)) (car records))
	(else (assoc key (cdr records)))))
(define (insert! key value table)
  (let ((record (assoc1 key (cdr table))))
    (if record
    	(set-cdr! record value)
    	(set-cdr! table
		  (cons (cons key value) (cdr table))))))
(define (make-table)
  (list '*table*))

;; Two-Dimensional Tables
(define (lookup key-1 key-2 table)
  (let ((subtable (assoc1 key-1 (cdr table))))
    (if subtable
    	(let ((record (assoc1 key-2 (cdr subtable))))
    	  (if record
	      (cdr record)
	      #f))
    	#f)))
(define (insert! key-1 key-2 value table)
  (let ((subtable (assoc1 key-1 (cdr table))))
    (if subtable
    	(let ((record (assoc1 key-2 (cdr subtable))))
    	  (if record
	      (set-cdr! record value)
	      (set-cdr! subtable
			(cons (cons key-2 value)
			      (cdr subtable)))))
    	(set-cdr! table
		  (cons (list key-1
			      (cons key-2 value))
			(cdr table))))))

;; Local tables
(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc1 key-1 (cdr local-table))))
        (if subtable
	    (let ((record (assoc1 key-2 (cdr subtable))))
	      (if record
		  (cdr record)
		  #f))
	    #f)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc1 key-1 (cdr local-table))))
        (if subtable
	    (let ((record (assoc1 key-2 (cdr subtable))))
	      (if record
		  (set-cdr! record value)
		  (set-cdr! subtable
			    (cons (cons key-2 value)
				  (cdr subtable)))))
	    (set-cdr! local-table
		      (cons (list key-1
				  (cons key-2 value))
			    (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
	    ((eq? m 'insert-proc!) insert!)
	    (else (error "Unknown operation -- TABLE" m))))
    dispatch))

;;; Exercise 3.24
(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc1 key-1 (cdr local-table))))
        (if subtable
	    (let ((record (assoc1 key-2 (cdr subtable))))
	      (if record
		  (cdr record)
		  #f))
	    #f)))
    (define (assoc key records)
      (cond ((null? records) #f)
            ((same-key? key (caar records)) (car records))
            (else (assoc key (cdr records)))))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc1 key-1 (cdr local-table))))
        (if subtable
	    (let ((record (assoc1 key-2 (cdr subtable))))
	      (if record
		  (set-cdr! record value)
		  (set-cdr! subtable
			    (cons (cons key-2 value)
				  (cdr subtable)))))
	    (set-cdr! local-table
		      (cons (list key-1
				  (cons key-2 value))
			    (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
	    ((eq? m 'insert-proc!) insert!)
	    (else (error "Unknown operation -- TABLE" m))))
    dispatch))

;; #;63> ((tbl1 'insert-proc!) 'math '+ 1)
;; ok
;; #;67> tbl1
;; #<procedure (dispatch m)>
;; #;68> ((tbl1 'lookup-proc) 'math '+)
;; 1
;; #;75>

;;; Exercise 3.25 - Generalized to n keys.
(define (make-table)
  (list '*table*))
(define (insert! table value key . keys)
  (if (null? keys)
      (let ((record (assoc key (cdr table))))
        (if record
            (set-cdr! record value)
            (set-cdr! table
                      (cons (cons key value)
                            (cdr table)))))
      (let ((subtable (assoc key (cdr table))))
        (if subtable
            (cond ((list? subtable)
                   (apply insert! subtable value (car keys) (cdr keys)))
                  ((pair? subtable)
                   (set-cdr! subtable '())
                   (apply insert! subtable value (car keys) (cdr keys))))
            (begin
              (set-cdr! table (cons (list key)
                                    (cdr table)))
              (apply insert! (cadr table) value (car keys) (cdr keys)))))))
(define (lookup table key . keys)
  (if (null? keys)
      (let ((record (assoc key (cdr table))))
        (if record
            (cdr record)
            #f))
      (let ((subtable (assoc key (cdr table))))
        (if subtable
            (apply lookup subtable (car keys) (cdr keys))
            #f))))

;; #;285> (insert! tbl1 5 'a 'e)
;; #;292> tbl1
;; (*table* (a (e . 5) (b (d . 4) (c . 3))))
;; #;293> (lookup tbl1 'a 'e)
;; 5
;; #;297> (lookup tbl1 'a 'b)
;; ((d . 4) (c . 3))
;; #;300> (lookup tbl1 'a 'b 'd)
;; 4
;; #;304> (lookup tbl1 'a 'b 'e)
;; #f
;; #;306> (lookup tbl1 'a 'b 'c)
;; 3

;;; Exercise 3.26 - (key . value) records stored as a binary tree.
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

(define (adjoin-set-tree! x set)
  (cond ((null? set)
         (make-tree x '() '()))
	((= (car x) (car (entry set))) set)
	((< (car x) (car (entry set)))
         (set-car! (cdr set) (adjoin-set-tree! x (left-branch set)))
         set)
	((> (car x) (car (entry set)))
         (set-car! (cddr set) (adjoin-set-tree! x (right-branch set)))
         set))) 
(define (make-table)
  (list '*table*))
(define (assoc key records)
  (cond ((null? records) #f)
        ((= key (car (entry records)))
         (entry records))
        ((< key (car (entry records)))
         (assoc key (left-branch records)))
        ((> key (car (entry records)))
         (assoc key (right-branch records)))))
(define (insert! table value key . keys)
  (if (null? keys)
      (let ((record (assoc key (cdr table))))
        (if record
            (set-cdr! record value)
            (set-cdr! table (adjoin-set-tree! (cons key value) (cdr table)))))
      (let ((subtable (assoc key (cdr table))))
        (if subtable
            (cond ((list? subtable)
                   (apply insert! subtable value (car keys) (cdr keys)))
                  ((pair? subtable)
                   (set-cdr! subtable (adjoin-set-tree! (cons key value) '()))
                   (apply insert! subtable value (car keys) (cdr keys))))
            (begin
              (set-cdr! table (adjoin-set-tree! (cons key '()) (cdr table)))
              (apply insert! (cadr table) value (car keys) (cdr keys))))))
  'ok)
(define (lookup table key . keys)
  (if (null? keys)
      (let ((record (assoc key (cdr table))))
        (if record
            (cdr record)
            #f))
      (let ((subtable (assoc key (cdr table))))
        (if subtable
            (apply lookup subtable (car keys) (cdr keys))
            #f))))

;; #;52> (lookup tbl1 0 1 0)
;; a
;; #;53> (lookup tbl1 0 2)
;; a
;; #;55> (lookup tbl1 0 1 1)
;; b
;; #;58> (lookup tbl1 1)
;; c

;;; Exercise 3.27 - Memoization
;;; just for grins going to use a 1D table made of binary tree elements.
(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table (adjoin-set-tree! (cons key value) (cdr table))))))
(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        #f)))

(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((previously-computed-result (lookup x table)))
        (or previously-computed-result
            (let ((result (f x)))
              (insert! x result table)
              result))))))
(define memo-counter 0)
(define memo-fib
  (memoize (lambda (n)
             (set! memo-counter (+ memo-counter 1))
             (cond ((= n 0) 0)
                   ((= n 1) 1)
                   (else (+ (memo-fib (- n 1))
                            (memo-fib (- n 2))))))))
(define fib-counter 0)
(define (fib n)
  (set! fib-counter (+ fib-counter 1))
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))
;;; (memoize fib) does not work because the recursive calls to fib call the actual fib procedure, not the
;;; memoized lambda that has access to the lookup table in its environment.

;;; Section 3.3.4 - A simulator for digital circuits.
(define (call-each procedures)
  (if (null? procedures)
      'done-call-each
      (begin
        ((car procedures))
        (call-each (cdr procedures)))))
(define (make-wire)
  (let ((signal-value 0) (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin
            (set! signal-value new-value)
            (call-each action-procedures))
          'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            ((eq? m 'get-procs) action-procedures)
            (else (error "Unknown operation -- WIRE" m))))
    dispatch))

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)
(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))
(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)
(define (logical-and s1 s2)
  (cond ((and (= s1 1) (= s2 1)) 1)
        ((or (= s1 0) (= s2 0)) 0)
        (else "Invalid signals" (list s1 s2))))
;;; Exercise 3.28
(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
           (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a1 or-action-procedure)
  'ok)

(define (logical-or s1 s2)
  (cond ((or (= s1 1) (= s2 1)) 1)
        ((and (= s1 0) (= s1 0)) 0)
        (else (error "Invalid signal" (list s1 s2)))))


;; (define (or-gate a b d)
;;   (let ((a1 (make-wire))
;;         (b1 (make-wire))
;;         (c  (make-wire)))
;;     (inverter a a1)
;;     (inverter b b1)
;;     (and-gate a1 b1 c)
;;     (inverter c d)
;;     'ok))

(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s))
  'ok)

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))


;; ;;; Exercise 3.30
(define (ripple-carry-adder a  b  s  c)
  (define (adder-iter as bs ss c)
    (let ((cn (make-wire)))
      (if (null? (cdr a))
          (set-signal! cn 0)
          (begin
            (full-adder (car as) (car bs) c (car ss) cn)
            (adder-iter (cdr as) (cdr bs) cn (cdr ss))))))
  (adder-iter a b s c))




(define (get-signal wire)
  (wire 'get-signal))
(define (get-procs wire)
  (wire 'get-procs))
(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))
(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))
(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))



;;; Exercise 3.31 - I believe the only thing that should change is you would not get the initial printout if the actions were not initialized. I will come back and test this. No need to test it. I stand corrected. The initialization is needed to run the inner defines in and-gate, or-gate, inverter. Without the initializiation, after-delay would not be called and the actions would no be added to the agenda.

;;; Confirmed :
;; #;70> (propagate)
;; mysum 8 New-value = 1
;; done
;;; With ((car procedures)) commented out.
;; #;75> (propagate)
;; done
;; #;80> mysum
;; #<procedure (dispatch m)>
;; #;81> (get-signal mysum)
;; 0

(define (make-time-segment time queue)
  (cons time queue))
(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))

(define (make-agenda) (list 0))
(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)
(define (current-time agenda) (car agenda))
(define (set-current-time! agenda time)
  (set-car! agenda time))
(define (segments agenda) (cdr agenda))
(define (set-segments! agenda segments)
  (set-cdr! agenda segments))
(define (first-segment agenda) (car (segments agenda)))
(define (rest-segments agenda) (cdr (segments agenda)))
(define (empty-agenda? agenda) (null? (segments agenda)))

(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (car segments)))))
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
        (insert-queue! (segment-queue (car segments))
                       action)
        (let ((rest (cdr segments)))
          (if (belongs-before? rest)
              (set-cdr! segments
                        (cons (make-new-time-segment time action)
                              (cdr segments)))
              (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments! agenda
                       (cons (make-new-time-segment time action)
                             segments))
        (add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
        (set-segments! agenda (rest-segments agenda)))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty -- FIRST-AGENDA-ITEM")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))


(define input-1 (make-wire))
(define input-2 (make-wire))
(define mysum (make-wire))
(define mycarry (make-wire))

(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (display name)
                 (display " ")
                 (display (current-time the-agenda))
                 (display " New-value = ")
                 (display (get-signal wire))
                 (newline))))

(probe 'mysum mysum)
(probe 'mycarry mycarry)

(half-adder input-1 input-2 mysum mycarry)
(set-signal! input-1 1)

;;; Section 3.3.5 - Propagation of Constraints/Constraint Networks
(define (celsius-fahrenheit-converter c f)
  (let ((u (make-connector))
        (v (make-connector))
        (w (make-connector))
        (x (make-connector))
        (y (make-connector)))
    (multiplier c w u)
    (multiplier v x u)
    (adder v y f)
    (constant 9 w)
    (constant 5 x)
    (constant 32 y)
    'ok))

(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond ((and (has-value? a1) (has-value? a2))
           (set-value! sum
                       (+ (get-value a1) (get-value a2))
                       me))
          ((and (has-value? a1) (has-value? sum))
           (set-value! a2
                       (- (get-value sum) (get-value a1))
                       me))
          ((and (has-value? a2) (has-value? sum))
           (set-value! a1
                       (- (get-value sum) (get-value a2))
                       me))))
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown Request -- ADDER" request))))
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)

(define (inform-about-value constraint)
  (constraint 'I-have-a-value))
(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))

(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond ((or (and (has-value? m1) (= (get-value m1) 0))
               (and (has-value? m2) (= (get-value m2) 0)))
           (set-value! product 0 me))
          ((and (has-value? m1) (has-value? m2))
           (set-value! product
                       (* (get-value m1) (get-value m2))
                       me))
          ((and (has-value? product) (has-value? m1))
           (set-value! m2
                       (/ (get-value product) (get-value m1))
                       me))
          ((and (has-value? product) (has-value? m2))
           (set-value! m1
                       (/ (get-value product) (get-value m2))
                       me))))
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- MULTIPLIER" request))))
  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)

(define (constant value connector)
  (define (me request)
    (error "Unknown request -- CONSTANT" request))
  (connect connector me)
  (set-value! connector value me)
  me)

(define (probe name connector)
  (define (print-probe value)
    (newline)
    (display "Probe: ")
    (display name)
    (display " = ")
    (display value)
    (newline))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value)
    (print-probe "?"))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- PROBE" request))))
  (connect connector me)
  me)

(define (make-connector)
  (let ((value #f) (informant #f) (constraints '()))
    (define (set-my-value newval setter)
      (cond ((not (has-value? me))
             (set! value newval)
             (set! informant setter)
             (for-each-except setter
                              inform-about-value
                              constraints))
            ((not (= value newval))
             (error "Contradiction" (list value newval)))
            (else 'ignored)))
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
          (begin (set! informant #f)
                 (for-each-except retractor
                                  inform-about-no-value
                                  constraints))
          'ignored))
    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
          (set! constraints
            (cons new-constraint constraints)))
      (if (has-value? me)
          (inform-about-value new-constraint))
      'done)
    (define (me request)
      (cond ((eq? request 'has-value?)
             (if informant #t #f))
            ((eq? request 'value) value)
            ((eq? request 'set-value!) set-my-value)
            ((eq? request 'forget) forget-my-value)
            ((eq? request 'connect) connect)
            (else (error "Unknown operation -- CONNECTOR" request))))
    me))

(define (for-each-except exception procedure list)
  (define (loop items)
    (cond ((null? items) 'done)
          ((eq? (car items) exception) (loop (cdr items)))
          (else (procedure (car items))
                (loop (cdr items)))))
  (loop list))

(define (has-value? connector)
  (connector 'has-value?))
(define (get-value connector)
  (connector 'value))
(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))
(define (forget-value! connector retractor)
  ((connector 'forget) retractor))
(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))

(define C (make-connector))
(define F (make-connector))
(celsius-fahrenheit-converter C F)

;; #;667> (forget-value! C 'user)
;; ignored
;; #;670> (set-value! F 212 'user)
;; done
;; #;671> (probe "Celsius temp" C)
;; #<procedure (me request)>
;; #;674> (probe "Fahrenheit temp" F)
;; #<procedure (me request)>
;; #;675> (set-value! F 212 'user)

;; Probe: Fahrenheit temp = 212
;; Probe: Celsius temp = 100done

;;; Exercise 3.33
(define (averager a b c)
  (let ((u (make-connector))
        (v (make-connector)))
    (constant 2 v)
    (multiplier c v u)
    (adder a b u)))

;; #;765> (let ((a (make-connector))
;;              (b (make-connector)) (c (make-connector)))
;;          (averager a b c)
;;          (probe "Average : " c) (set-value! a 5 'user) (set-value! b 10 'user))

;; Probe: Average :  = 7.5
;; done

;;; Exercise 3.34
(define (squarer a b)
  (multiplier a a b))

;;; Should work from a -> b, but setting b will not give the square root properly.
;; #;808> (let ((a (make-connector))
;;              (b (make-connector)))
;;          (probe "a : " a) (probe "b : " b) (squarer a b) (set-value! a 2 'user))

;; Probe: b :  = 4

;; Probe: a :  = 2
;;; Setting b does not work because the product has a value, but the two inputs have no value so no condition is satisfied.

;;; Exercise 3.35
(define (squarer a b)
  (define (process-new-value)
    (if (has-value? b)
        (if (< (get-value b) 0)
            (error "square less than 0 -- SQUARER" (get-value b))
            (set-value! a (sqrt (get-value b)) me))
        (set-value! b (* (get-value a) (get-value a)) me)))
  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- SQUARER" request))))
  (connect a me)
  (connect b me)
  me)

;; #;1017> (let ((a (make-connector))
;;              (b (make-connector)))
;;          (probe "a : " a) (probe "b : " b) (squarer a b) (set-value! b 16 'user))

;; Probe: a :  = 4.0

;; Probe: b :  = 16
;; done
;; #;1033> (let ((a (make-connector))
;;              (b (make-connector)))
;;          (probe "a : " a) (probe "b : " b) (squarer a b) (set-value! b 15 'user))

;; Probe: a :  = 3.87298334620742

;; Probe: b :  = 15
;; done
;; #;1038> (let ((a (make-connector))
;;              (b (make-connector)))
;;          (probe "a : " a) (probe "b : " b) (squarer a b) (set-value! a (sqrt 15) 'user))

;; Probe: b :  = 15.0

;; Probe: a :  = 3.87298334620742
;; done

(define (celsius-fahrenheit-converter x)
  (c+ (c* (c/ (cv 9) (cv 5))
          x)
      (cv 32)))

(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))

(define (c- x y)
  (let ((z (make-connector)))
    (adder (y z x))
    z))
(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))
(define (c/ x y)
  (let ((z (make-connector)))
    (multiplier y z x)
    z))
(define (cv x)
  (let ((z (make-connector)))
    (constant x z)
    z))

;; (define C (make-connector))
;; (define F (celsius-fahrenheit-converter C))
;; (probe "Celsisus : " C)
;; (probe "Fahrenheit : " F)
;; #;1218> (set-value! C 100 'user)

;; Probe: Celsisus :  = 100

;; Probe: Fahrenheit :  = 212.0
;; done

;;; The complexities introduced by assignment become even more problematic in the presence of concurrency.
;;; Indeterminancy in the order of events can pose serious problems in the design of concurrent systems.
;;; Footnote 36 : cache-coherence protocols are required to ensure various processors maintain a consistent view of memory contents.

;;; ensure that a concurrent system produces the same result "as if" the processes had run concurrently.
;;; concurrent programs are inherently non-deterministic.

;;; A more practical approach to concurrent systems is to devise general mechanisms that allow us to constrain the interleaving of concurrent processes so that we can be sure the program behavior is correct.

;;; Serializer - Using serialization to control access to shared variables.
;; (define (parallel-execute . thunks)
;;   (let ((threads (map thunk->thread thunks)))
;;     (lambda () (for-each thread-terminate! threads))))

;; (define (thunk->thread thunk)
;;   (let ((thread (make-thread thunk))) (thread-start! thread) thread))

;; (define x 10)
;; (parallel-execute (lambda () (set! x (* x x)))
;;                   (lambda () (set! x (+ x 1))))

;;; Exercises 3.36-3.46 in Noteboook.

;;; Exercise 3.47 - Semaphore of size n
;;; Even though a semaphore is a generalization of a mutex, I think we still need a mutex to serialize access to the
;;; count. Otherwise n threads could all acquire at the same time and our final count would be (0 + 1) and any
;;; subsequent threads would be able to exceed the n of the semaphore.
;;; a) In terms of mutexes.
(define (make-semaphore n)
  (let ((lock (make-mutex))
        (i 0))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
             (lock 'acquire)
             (if (>= i n)
                 (begin
                   (lock 'release)
                   (the-semaphore 'acquire))
                 (begin
                   (set! i (+ i 1))
                   (lock 'release))))
            ((eq? m 'release)
             (lock 'acquire)
             (set! i (- i 1))
             (lock 'release))))
    the-semaphore))

;;; If test-and-set! is done atomically then I don't think we need to acquire a lock and serialize access to the count.
;;; b) atomically
(define (make-semaphore n)
  (let ((cell (list 0)))
    (define (test-and-set! cell)
      (without-interrupts
       (lambda ()
         (if (< (car cell) n)
             (begin
               (set-car! cell (+ (car cell) 1))
               #f)
             #t))))
    (define (clear! cell)
      (without-interrupts
       (lambda ()
         (set-car! cell (- (car cell) 1)))))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-semaphore 'acquire)))
            ((eq? m 'release) (clear cell))))
    the-semaphore))

;;; Exercises 3.48 - 3.49 in Noteboook.

;;; Streams 3.5
;; (define (cons-stream a b)
;;   (cons a (delay b)))
;; (define (stream-car stream)
;;   (display stream) (newline)
;;   (car stream))
;; (define (stream-cdr stream)
;;   (force (cdr stream)))

;; (use streams)

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))
(define (stream-ref stream n)
  (if (= n 0)
      (stream-car stream)
      (stream-ref (stream-cdr stream) (- n 1))))

;; (define (force delayed-object)
;;   (delayed-object))

;; (define (memo-proc proc)
;;   (let ((already-run? #f) (result #f))
;;     (lambda ()
;;       (if (not already-run?)
;;           (begin
;;             (set! result (proc))
;;             (set! already-run? #t)
;;             result)
;;           result))))

;; (define (delay p)
;;   (memo-proc (lambda ()
;;                (p))))

;;; Exercise 3.50
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (begin
         (apply proc (map stream-car argstreams)))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

;;; Exercise 3.51
(define (show x)
  (display x) (newline)
  x)

(define x (stream-map show (stream-enumerate-interval 0 10)))

;;; I was getting different behavior in chicken than what I found online. Using mit-scheme now. Probable should have
;;; been from the start
;; 1 ]=> (stream-ref x 5)
;; 1
;; 2
;; 3
;; 4
;; 5
;; ;Value: 5

;; 1 ]=> (stream-ref x 7)
;; 6
;; 7
;; ;Value: 7

(define sum 0)
(define (accum x)
  (set! sum (+ x sum))
  sum)

(define seq (stream-map accum (stream-enumerate-interval 1 20)))
(define y (stream-filter even? seq))
(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
                         seq))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))
(define (display-line x)
  (newline)
  (display x))

;;; Section 3.5.2 Infinite Streams
(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))
(define (divisible? x y) (= (remainder x y) 0))
(define no-sevens
  (stream-filter (lambda (x) (not (divisible? x 7)))
                 integers))
(define (fibgen a b)
  (cons-stream a (fibgen b (+ a b))))
(define fibs (fibgen 0 1))

(define (sieve stream)
  (cons-stream
   (stream-car stream)
   (sieve (stream-filter
           (lambda (x)
             (not (divisible? x (stream-car stream))))
           (stream-cdr stream)))))
(define primes (sieve (integers-starting-from 2)))

(define ones (cons-stream 1 ones))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define integers (cons-stream 1 (add-streams ones integers)))

(define fibs
  (cons-stream 0
               (cons-stream 1
                            (add-streams (stream-cdr fibs)
                                         fibs))))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define double (cons-stream 1 (scale-stream double 2)))

(define primes
  (cons-stream
   2
   (stream-filter prime? (integers-starting-from 3))))

(define (prime? n)
  (define (iter ps)
    (cond ((> (square (stream-car ps)) n) true)
          ((divisible? n (stream-car ps)) false)
          (else (iter (stream-cdr ps)))))
  (iter primes))

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

;;; Need to use (stream-cdr integers) to get nth element to be n+1!
(define factorials (cons-stream 1 (mul-streams factorials (stream-cdr integers))))
;;1 ]=> (stream-ref factorials 0)


;; ;Value: 1

;; 1 ]=> (stream-ref factorials 1)

;; ;Value: 2

;; 1 ]=> (stream-ref factorials 2)

;; ;Value: 6

;; 1 ]=> (stream-ref factorials 3)

;; ;Value: 24

;; 1 ]=> (stream-ref factorials 4)

;; ;Value: 120

;; 1 ]=> (stream-ref factorials 5)

;; ;Value: 720

;; Exercise 3.55
(define (partial-sums s)
  (define (iter p s)
    (cons-stream p (stream-map (lambda (x) (+ p x)) (partial-sums s))))
  (iter (stream-car s) (stream-cdr s)))

;; above is kind of gross, this is very elegant but kind of hard to grasp on the first go-round.
(define (partial-sums s)
  (add-streams s (cons-stream 0 (partial-sums s))))

;;1 ]=> (stream-ref (partial-sums integers) 1)
;;
;;;Value: 3
;;
;;1 ]=> (stream-ref (partial-sums integers) 2)
;;
;;;Value: 6
;;
;;1 ]=> (stream-ref (partial-sums integers) 3)
;;
;;;Value: 10
;;
;;1 ]=> (stream-ref (partial-sums integers) 4)
;;
;;;Value: 15

;; Exercise 3.56
(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream s1car (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream s2car (merge s1 (stream-cdr s2))))
                 (else
                  (cons-stream s1car
                               (merge (stream-cdr s1)
                                      (stream-cdr s2)))))))))

(define S (cons-stream 1 (merge (scale-stream S 2) (merge (scale-stream S 3) (scale-stream S 5)))))


;;; Exercise 3.58 - In Notebook

;;; Exercise 3.59
(define (div-streams s1 s2)
  (stream-map / s1 s2))

(define (integrate-series as)
  (mul-streams (div-streams ones integers) as))

(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

(define cosine-series
  (cons-stream 1 (integrate-series (scale-stream sine-series -1))))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

;;; This is a freaking elegant representation.

;;; Exercise 3.60
(define (add-series s1 s2)
  (add-streams s1 s2))
(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (add-streams (mul-series (cons-stream 0 (stream-cdr s1)) s2)
                            (mul-series s1 (cons-stream 0 (stream-cdr s2))))))

;;; This seems to work but seems pretty slow.
(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (add-streams (scale-stream (stream-cdr s2) (stream-car s1))
                            (mul-series (stream-cdr s1) s2))))
;;; This is much faster.
;;; This exercise took me longer than I would like to admit.

;;; Exercise 3.61
(define (invert-unit-series s)
  (cons-stream 1
               (scale-stream (mul-series (stream-cdr s) (invert-unit-series s)) -1)))

;;; Exercise 3.62
(define (div-series s1 s2)
  (if (eq? (stream-car s2) 0)
      (error "Divisor has a constant term of zero -- DIV-SERIES" (list s1 s2))
      (mul-series s1 (scale-stream (invert-unit-series s2) (stream-car s2)))))

(define tangent-series (div-series sine-series cosine-series))

;;1 ]=> (display-stream tangent-series)
;;
;;0
;;1
;;0
;;1/3
;;0
;;2/15
;;0
;;17/315
;;0
;;62/2835
;;0


;;; Section 3.5.3 - Exploiting the Stream Paradigm
;;; Represent state as a "timeless" stream of values rather than as a set of variables to be updated.
(define (average a b)
  (/ (+ a b) 2))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)

(define (pi-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))

(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))


(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform
                             (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))


;; Exercise 3.63 -- In Notebook.
;;(define (sqrt-stream x)
;;  (cons-stream 1.0
;;  			   (stream-map (lambda (guess) (sqrt-improve guess x)) (sqrt-stream x))))


;; Exercise 3.64
(define (stream-limit s d)
  (let ((s1 (stream-car s))
        (s2 (stream-car (stream-cdr s))))
    (if (<= (abs (- s1 s2)) d)
    	s2
    	(stream-limit (stream-cdr s) d))))

;;1 ]=> (stream-limit (accelerated-sequence euler-transform pi-stream) 0.00001)
;;
;;;Value: 3.1415927140337785
;;
;;1 ]=> (stream-limit (accelerated-sequence euler-transform pi-stream) 0.0000001)
;;
;;;Value: 3.1415926539752927
;;
;;1 ]=> (stream-limit (accelerated-sequence euler-transform pi-stream) 0.000000001)
;;
;;;Value: 3.1415926535911765

;; Exercise 3.65
(define (ln-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (ln-summands (+ n 1)))))

(define ln-stream
  (partial-sums (ln-summands 1)))

;;; ln-stream hits the recursion limit for 6 sig digits.
;;; euler-transform converges much quicker
;;; accelerated transform converges the quickest

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

;;; Exercise 3.66 - In Notebook.

;;; Exercise 3.67
;; (define (pairs s t)
;;   (cons-stream
;;    (list (stream-car s) (stream-car t))
;;    (interleave
;;     (stream-map (lambda (x) (list (stream-car s) x))
;;                 (stream-cdr t))
;;     (pairs (stream-cdr s) t))))

;; (define (pairs s t) 
;;   (cons-stream
;;    (list (stream-car s) (stream-car t))
;;    (interleave
;;     (stream-map (lambda (x) 
;;                   (list (stream-car s) x))
;;                 (stream-cdr t))
;;     (interleave 
;;      (stream-map (lambda (x) 
;;                   (list x (stream-car t)))
;;                  (stream-cdr s))
;;      (pairs (stream-cdr s) (stream-cdr t))))))

;;; Exercise 3.68 - In Notebook. Doesn't work because pairs is evaluated over and over and is never delayed.

;;; Exercise 3.69
(define (triples s t u)
  (cons-stream
   (list (stream-car s) (stream-car t) (stream-car u))
   (interleave
    (stream-map (lambda (x) (cons (stream-car s) x))
                (stream-cdr (pairs t u)))
    (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))

(define pythag
  (stream-filter (lambda (x)
                   (= (+ (square (car x)) (square (cadr x)))
                      (square (caddr x))))
                 (triples integers integers integers)))

;;; works but it aint pretty
;; 1 ]=> (display-stream pythag)

;; (3 4 5)
;; (6 8 10)
;; (5 12 13)
;; (9 12 15)
;; (8 15 17)
;; (12 16 20)
;; ;Aborting!: out of memory
;; ;GC #97: took:   0.27  (48%) CPU time,   0.28  (50%) real time; free: 4184
;; ;GC #98: took:   0.28 (100%) CPU time,   0.28 (100%) real time; free: 4378
;; ;GC #99: took:   0.27 (100%) CPU time,   0.28 (100%) real time; free: 4378

;;; Exercise 3.70
;;; The trick here is turn turn merge-weighted into a cross  between merge and
;;; interleave.
(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((p1 (stream-car s1))
               (p2 (stream-car s2)))
           (let ((w1 (weight p1))
                 (w2 (weight p2)))
             (cond ((< w1 w2)
                    (cons-stream p1 (merge-weighted (stream-cdr s1) s2 weight)))
                   (else
                    (cons-stream p2
                                 (merge-weighted s1
                                                 (stream-cdr s2)
                                                 weight)))))))))

(define (weighted-pairs s t weight)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
    weight)))

;; 1 ]=> (display-stream (weighted-pairs integers integers (lambda (pair) (+ (car pair) (cadr pair)))))

;; (1 1)
;; (1 2)
;; (2 2)
;; (1 3)
;; (2 3)
;; (1 4)
;; (3 3)
;; (2 4)
;; (1 5)
;; (3 4)
;; (2 5)
;; (1 6)
;; (4 4)
;; (3 5)
;; (2 6)
;; (1 7)
;; (4 5)
;; (3 6)
;; (2 7)
;; (1 8)
;; (5 5)
;; (4 6)
;; (3 7)
;; (2 8)
;; (1 9)
;; (5 6)
;; (4 7)
;; (3 8)
;; (2 9)
;; (1 10)
;; (6 6)
;; (5 7)
;; (4 8)
;; (3 9)
;; (2 10)
;; (1 11)

;;; b)
;; (define integers-no235 (stream-filter (lambda (x)
;;                                         (and (not (divisible? x 2))
;;                                              (not (divisible? x 3))
;;                                              (not (divisible? x 5)))) integers))
;; (display-stream (weighted-pairs integers-no235 integers-no235 (lambda (p)
;;                                                                 (let ((i (car p))
;;                                                                       (j (cadr p)))
;;                                                                   (+ (* 2 i)
;;                                                                      (* 3 j)
;;                                                                      (* 5 i j))))))

;;  ]=> (display-stream (weighted-pairs integers-no235 integers-no235 (lambda (p)
;;                                                                 (let ((i (car p))
;;                                                                       (j (cadr p)))
;;                                                                   (+ (* 2 i)
;;                                                                      (* 3 j)
;;                                                                      (* 5 i j))))))

;; (1 1)
;; (1 7)
;; (1 11)
;; (1 13)
;; (1 17)
;; (1 19)
;; (1 23)
;; (1 29)
;; (1 31)
;; (7 7)
;; (1 37)
;; (1 41)
;; (1 43)
;; (1 47)
;; (1 49)
;; (1 53)
;; (7 11)
;; (1 59)
;; (1 61)
;; (7 13)
;; (1 67)
;; (1 71)
;; (1 73)
;; (1 77)
;; (1 79)
;; (11 11)
;; (7 17)
;; (1 83)
;; (1 89)
;; (1 91)
;; (7 19)
;; (11 13)



;;; Exercise 3.71 - Ramanujan Numbers
;;; Numbers that can be expressed as the sum of two cubes in more than one way.
(define (ramanujan-weight p)
  (+ (expt (car p) 3)
     (expt (cadr p) 3)))

(define sum-cubes
  (weighted-pairs integers integers ramanujan-weight))

(define (ramanujans s1 s2)
  (let ((w1 (ramanujan-weight (stream-car s1)))
        (w2 (ramanujan-weight (stream-car s2))))
    (if (= w1 w2)
        (cons-stream
         w1
         (ramanujans (stream-cdr s1) (stream-cdr s2)))
        (ramanujans (stream-cdr s1) (stream-cdr s2)))))

;; 1 ]=> (display-stream (ramanujans sum-cubes (stream-cdr sum-cubes)))

;; 1729
;; 4104
;; 13832
;; 20683
;; 32832
;; 39312

;;; Exercise 3.72 - Numbers that can be written as the sum of two squares in three different ways
(define (ex3.72-weight p)
  (+ (expt (car p) 2)
     (expt (cadr p) 2)))

(define sum-of-squares
  (weighted-pairs integers integers ex3.72-weight))

(define (ex3.72-search s1 s2 s3)
  (let ((p1 (stream-car s1))
        (p2 (stream-car s2))
        (p3 (stream-car s3)))
    (let ((w1 (ex3.72-weight p1))
          (w2 (ex3.72-weight p2))
          (w3 (ex3.72-weight p3)))
      (if (= w1 w2 w3)
          (cons-stream
           (list w1 p1 p2 p3)
           (ex3.72-search (stream-cdr s1) (stream-cdr s2) (stream-cdr s3)))
          (ex3.72-search (stream-cdr s1) (stream-cdr s2) (stream-cdr s3))))))

(define ex3.72-numbers
  (ex3.72-search sum-of-squares (stream-cdr sum-of-squares) (stream-cdr (stream-cdr sum-of-squares))))

;; (325 (10 15) (6 17) (1 18))
;; (425 (13 16) (8 19) (5 20))
;; (650 (17 19) (11 23) (5 25))
;; (725 (14 23) (10 25) (7 26))
;; (845 (19 22) (13 26) (2 29))
;; (850 (15 25) (11 27) (3 29))
;; (925 (21 22) (14 27) (5 30))


(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)

;;; Exercise 3.73
(define (RC R C dt)
  (lambda (i v0)
    (add-streams (scale-stream i R)
                 (scale-stream (integral i v0 dt)
                               (/ 1 C)))))


;;; Exercise 3.74
(define (list-to-lazy lst)
  (if (null? lst)
      the-empty-stream
      (begin
        (cons-stream
         (car lst)
         (list-to-lazy (cdr lst))))))

(define sense-data (list-to-lazy (list 1 2 1.5 1 0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4)))

(define (sign-change-detector a b)
  (cond ((and (< b 0) (> a 0)) 1)
        ((and (< a 0) (> b 0)) -1)
        (else
         0)))
(define (make-zero-crossings input-stream last-value)
  (cons-stream
   (sign-change-detector (stream-car input-stream) last-value)
   (make-zero-crossings (stream-cdr input-stream) (stream-car input-stream))))

(define zero-crossings (make-zero-crossings sense-data 0))

(define zero-crossings
  (stream-map sign-change-detector sense-data (cons-stream 0 sense-data)))

;; ;;; Exercise 3.75
(define (make-zero-crossings input-stream last-value last-avg)
  (let ((avpt (/ (+ (stream-car input-stream) last-value) 2)))
    (newline) (display "avpt : ") (display  (exact->inexact avpt)) (newline)
    (cons-stream (sign-change-detector avpt last-avg)
                 (make-zero-crossings (stream-cdr input-stream)
                                      (stream-car input-stream)
                                      avpt))))
(define zero-crossings (make-zero-crossings sense-data 0 0))

;;; Pretty sure the error is passing avpt as 'last-value'. Instead of taking the average of the stream values,
;;; the avpt binding is taking the average of the current point with respect to the last average point

;; 0 1  2  1.5   1   0.5   -0.1    -2 -3 -2 -0.5 0.2 3 4
;;  0.5 1.25 1.375                        Louis Reasoner's method
;;  0.5 1.5 1.75 1.25  .75  .2            Correct Averaging


;;; Exercise 3.76 - Smooth Function
(define (average n m)
  (/ (+ n m) 2))
(define (smooth s func)
  (let ((s1 (stream-car s))
        (s2 (stream-car (stream-cdr s))))
    (cons-stream
     (func s1 s2)
     (smooth (stream-cdr s) func))))

(define (make-zero-crossings input-stream last-value)
  (cons-stream
   (sign-change-detector (stream-car input-stream) last-value)
   (make-zero-crossings (stream-cdr input-stream) (stream-car input-stream))))

(define zero-crossings (make-zero-crossings (smooth sense-data average) 0))

;; 1 ]=> (display-stream zero-crossings)

;; 0
;; 0
;; 0
;; 0
;; 0
;; -1
;; 0
;; 0
;; 0
;; 0
;; 1
;; 0


(define (integral delayed-integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (let ((integrand (force delayed-integrand)))
                   (add-streams (scale-stream integrand dt)
                                int))))
  int)

(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

;;1 ]=> (stream-ref (solve (lambda (y) y) 1 0.001) 1000)
;;
;;;Value: 2.716923932235896

;; Exercise 3.77 -- Need to force the delayed integrand in a let binding, then delay the argument
;; passed in the recursive call.
;; (define (integral delayed-integrand initial-value dt)
;;   (cons-stream initial-value
;;                (let ((integrand (force delayed-integrand)))  
;;                  (if (stream-null? integrand)
;;                      the-empty-stream
;;                      (integral (delay (stream-cdr integrand))
;;                                (+ (* dt (stream-car integrand))
;;                                   initial-value)
;;                                dt)))))

;; 1 ]=> (stream-ref (solve (lambda (y) y) 1 0.001) 1000)

;; ;Value: 2.716923932235896

;;; Exercise 3.78 -- solving a second order ODE using signal processing
(define (solve-2nd a b y0 dy0 dt)
  (define y  (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (add-streams (scale-stream dy a)
                           (scale-stream y b)))
  y)

;; 1 ]=> (stream-ref (solve-2nd 1 1 1 1 0.001) 1000)
;; (1 . #[promise 39])
;; (1 . #[promise 40])
;; (2 . #[promise 41])
;;Value: 3.793448578735687

;;; Exercise 3.79 - Generalization of 3.78 of d^2y/dt^2 = f(dy/dy, y)
(define (solve-2nd f y0 dy0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (stream-map f dy y))
  y)

;; 1 ]=> (stream-ref (solve-2nd (lambda (dy y) (+ (* 1 dy) (* 1 y))) 1 1 .001) 1000)

;; ;Value: 3.793448578735687

;;; Exercise 3.80 - A Series RLC circuit
(define (RLC R L C dt)
  (lambda (vc0 il0)
    (define vc (integral (delay dvc) vc0 dt))
    (define il (integral (delay dil) il0 dt))
    (define dvc (scale-stream il (- (/ 1 C))))
    (define dil (add-streams
                 (scale-stream vc (/ 1 L))
                 (scale-stream il (/ (- R) L))))
    (cons vc il)))

(define ex3.80-circuit (RLC 1 1 0.2 0.1))

;;; damping factor : 
;; 1 ]=> (* (/ 1 2.0) (sqrt (/ 0.2 1)))
;; ;Value: .22360679774997896

;;; This example is an underdamped system so the current should burst up, then swing below zero perhaps a few times,
;;; before trending towards 0. I won't paste it all here, but the solution appears correct with a quick double check.

;;; "Including dealys in procedure calls wreaks havoc with our ability to design programs that depend on the order
;;;  of events, such as programs that use assignment, mutate data, or perform input or output."

;;; Section 3.5.5 - Reimplementing monte carlo from a stream-processing point of view.
;;; Key Modularity Concern - Hide the internal state of a RNG from programs that use RNs.

;; (define random-numbers
;;   (cons-stream random-init
;;                (stream-map rand-update random-numbers)))

;; (define (map-successive-pairs f s)
;;   (cons-stream
;;    (f (stream-car s) (stream-car (stream-cdr s)))
;;    (map-successive-pairs f (stream-cdr (stream-cdr s)))))

;; (define cesaro-stream
;;   (map-successive-pairs (lambda (r1 r2) (= (gcd r1 r2) 1))
;;                         random-numbers))

(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
     (/ passed (+ passed failed))
     (monte-carlo
      (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

;; (define pi
;;   (stream-map (lambda (p) (sqrt (/ 6 p)))
;;               (monte-carlo cesaro-stream 0 0)))


;;; Exercise 3.81 - Re-implement Exercise 3.6 from a stream processing POV.
;;; Exercise 3.6 with rand-update redifined to a predictable update value
(define (rand-update x)
  (+ x 2))
(define rand
  (let ((x 0))
    (lambda (action)
      (define worker
	(lambda init
	  (if (null? init)
	      (begin
		(set! x (rand-update x))
		x)
	      (begin
		(set! x (car init))))))
      (cond ((eq? action 'generate)
	     (worker))
	    ((eq? action 'reset)
	     (lambda (i)
	       (worker i)))
	    (else
	     (error "unknown method chosen -- RAND" action))))))

(define cmd-stream (list-to-lazy (list '(reset 1) 'generate 'generate '(reset 2) 'generate 'generate)))

(define (rand-gen cmd-stream num)
  (let ((item (stream-car cmd-stream)))
    (cond ((and (pair? item) (eq? (car item) 'reset))
           (cons-stream (cadr item)
                        (rand-gen (stream-cdr cmd-stream) (cadr item))))
          ((eq? item 'generate)
           (let ((n (rand-update num)))
             (cons-stream
              n
              (rand-gen (stream-cdr cmd-stream) n))))
          (else
           (error "unknown command in stream -- RAND-GEN" item)))))

(define rand
  (rand-gen cmd-stream 0))

;; 1 ]=> (display-stream rand)

;; 1
;; 3
;; 5
;; 2
;; 4
;; 6

;;; Exercise 3.82 - Redo Exercise 3.5 in terms of streams
;;; Exercise - 3.5
;;; Tweaked the random-in-range so that a random decimal is given. This seems to produce much better results.
(define (random-in-range low high)
  (if (= low high)
      0
      (let ((range (- high low)))
        (+ low (+ (random range) (random 1.0))))))

(define (estimate-integral P x1 y1 x2 y2 trials)
  (exact->inexact (* (region-area x1 y1 x2 y2)
		     (monte-carlo trials
				  (lambda ()
				    (region-test P x1 y1 x2 y2))))))
(define (region-area x1 y1 x2 y2)
  (* (- x2 x1) (- y2 y1)))

(define (region-test P x1 y1 x2 y2)
  (let ((x (random-in-range x1 x2))
	(y (random-in-range y1 y2)))
    (P x y)))

(define (in-unit-circle? x y)
  (<= (+ (expt x 2.0) (expt y 2.0)) 1.0))

(define (build-integration-experiment P x1 y1 x2 y2)
  (cons-stream
   (region-test P x1 y1 x2 y2)
   (build-integration-experiment P x1 y1 x2 y2)))

(define (estimate-integral P x1 y1 x2 y2)
  (stream-map (lambda (x) (exact->inexact (* (region-area x1 y1 x2 y2) x)))
              (monte-carlo (build-integration-experiment P x1 y1 x2 y2) 0 0)))

;; 1 ]=> (stream-ref (estimate-integral in-unit-circle? -1 -1 1 1) 100000)

;; ;Value: 3.147168528314717

;; 1 ]=> (stream-ref (estimate-integral in-unit-circle? -1 -1 1 1) 1000000)

;; ;Value: 3.141912858087142
