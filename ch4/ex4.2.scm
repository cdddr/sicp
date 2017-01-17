;;; Exercise 4.2
;;; Modify the language so that procedure applications start with `call'.

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ;; moved application? above the assignemnt? and definition? sections
        ;; should work as is with the changes below.
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))        
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-f exp env))
        ((lambda? exp)
         (make-procedure (lambda-paramters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        (else
         (error "Unknown expression type -- EVAL" exp))))

;;; Modify application? to check for a tagged-list with 'call.
(define (application? exp) (tagged-list? exp 'call))
;;; The operator is now in the cadr position
(define (operator exp) (cadr exp))
;;; The operands are now in cddr
(define (operands exp) (cddr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))
