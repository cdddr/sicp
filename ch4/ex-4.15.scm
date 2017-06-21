;;; Given a one-argument procedure p and an object a, p is said to "halt" on a if evaluating the expression:
;;;     (p a)
;;; returns a value. Show that it is impossible to write a procedure "halts?" that correctly determines whether
;;; p halts on a for any procedure p and object a.

(define (run-forever) (run-forever))

(define (try p)
  (if (halts? p p)
      (run-forever)
      'halted))

;;; If (try try) is evaluated and (halts? try try) returns true, then the program will run forever. However,
;;; if (halts? try try) returns false, then the program will halt. So it is impossible in this case to write
;;; a procedure "halts?" that correctly determines whether or not a procedure will halt.
