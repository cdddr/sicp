;;;  -*- geiser-scheme-implementation: mit -*-
;;; Derived Expressions
;;;  cond
;; Extended Syntax
;; (cond ((assoc 'b '((a 1) (b 2))) => cadr)
;;       (else false))
(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-=>? clause)
  (eq? (cadr clause) '=>))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (cond ((cond-else-clause? first)
               (if (null? rest)
                   (sequence->exp (cond-actions first))
                   (error "ELSE clause isn't last -- COND->IF"
                          clauses)))
              ((cond-=>? first)
               (make-if (cond-predicate first)
                        (list (caddr first) (cond-predicate first))
                        (expand-clauses rest)))
              (else
               (make-if (cond-predicate first)
                        (sequence->exp (cond-actions first))
                        (expand-clauses rest)))))))


