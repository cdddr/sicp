(define (and? exp)
  (tagged-list? exp 'and))

(define (or? exp)
  (tagged-list? exp 'or))

(define (eval-or exp env)
  (define iter (exps env)
    (cond ((last-exp? exps)
           (eval (first-exp exps) env))
          ((eval (first-exp exps) env) #t)
          (else
           (iter (rest-exps exps) env))))
  (iter exp env))

(define (eval-and exp env)
  (define iter (exps env)
    (cond ((last-exp? exps)
           (eval (first-exp exps) env))
          ((not (eval (first-exp exps) env)) #f)
          (else
           (iter (rest-exps exps) env))))
  (iter exp env))

;; (define (eval-and exp env)
;;   )

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-paramters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((and? exp) (eval-and exp env))
        ((or? exp) (eval-or exp env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

