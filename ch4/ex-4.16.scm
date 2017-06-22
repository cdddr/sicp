;;; -*- geiser-scheme-implementation: mit -*-
(load "ch4.scm")
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (if (eq? (car vals) '*unassigned*)
                 (error "Value for variable is not yet assigned -- " var)
                 (car vals)))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- this is it" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (scan-out-defines body)
  "Takes a procedure body and transforms internal defines into let/set!"
  (define (scan body vars vals rest)
    (cond ((null? body)
           (if (null? vars)
               rest
               (append  (list 'let vars) vals rest)))
          ((definition? (car body))
           (let ((var (definition-variable (car body)))
                 (val (definition-value (car body))))
             (scan (cdr body)
                   (cons (list var (list 'quote '*unassigned*)) vars)
                   (cons (list 'set! var val) vals)
                   rest)))
          (else           
           (scan (cdr body)
                 vars
                 vals
                 (cons (car body) rest)))))
  (scan body '() '() '()))

;;; This should be installed in make-procedure, otherwise it will be run everytime procedure-body is run.
