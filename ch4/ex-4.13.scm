;; -*- geiser-scheme-implementation: mit -*-
;;; I only want to unbind variables in the current frame. Messing with variable bindings
;;; in enclosing environments sounds like a bad idea.
(define (make-unbound! var env)
  (let ((frame (first-frame env)))    
    (define (scan bindings)
      "This function will scan the cdr pointers for the variable"
      (cond ((or (null? bindings) (null? (cdr bindings)))
             (error "Unbound variable" var))
            ((eq? (caar bindings) var)
             (set-car! bindings (cadr bindings))
             (set-cdr! bindings (cddr bindings)))
            ((eq? (caadr bindings) var)
             (set-cdr! bindings (cddr bindings)))
            (else
             (scan (cdr bindings)))))
    (scan frame)))
