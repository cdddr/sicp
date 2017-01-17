;;;  -*- geiser-scheme-implementation: mit -*-
(define (make-let bindings body)
  (list 'let bindings body))
(define (let*? exp) (tagged-list exp 'let*))
(define (let*->nested-lets exp)
  (let ((bindings (let-bindings exp)))
    (if (null? bindings)
        (sequence->exp (let-body exp))
        (let ((first-bindings (car  bindings))
              (rest-bindings (cdr bindings)))
          (make-let (list first-bindings)
                    (let*->nested-lets
                     (list 'let* rest-bindings (sequence->exp (let-body exp)))))))))
;;; It should be sufficient to just install let*. The nested lets will be converted into nested lambdas which should
;;; manage our environment bindings correctly.
