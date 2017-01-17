;;; Exercise 4.1 - list-of-values that evaluates operands left-to-right regardless of underlying lisp.
;;; The important thing to control here is when (eval) is run. For left to right, we want eval to run on the
;;; first item
(define (list-of-values exps env)
  (define (iter exps operands)
    (if (no-operands? exps)
        operands
        (iter (rest-operands exps) (cons
                                    (eval (first-operand exps) env)
                                    operands))))
  (iter exps '()))

;;; For right to left, assuming exps is a list we could just reverse it:
(define (list-of-values exps env)
  (define (iter exps operands)
    (if (no-operands? exps)
        operands
        (iter (rest-operands exps) (cons
                                    (eval (first-operand exps) env)
                                    operands))))
  (iter (reverse exps) '()))

;;; Another way would be to use a let binding for the rest (defined 1st) and the first (defined 2nd). Then when we
;;; recurse down to the end of 'exps' we enter the let binding for 'first' and the eval takes place.
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((rest (list-of-values (rest-operands exps) env)))
        (let ((first (eval (first-operand exps) env)))
          (cons first rest)))))
