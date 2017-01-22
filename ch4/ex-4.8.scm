;;;  -*- geiser-scheme-implementation: mit -*-
;;; (let <var> <bindings> <body>)
(define (let? exp) (tagged-list? exp 'let))
(define (let-name exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      #f))
(define (let-named? exp)
  (let-name exp))
(define (let-body exp)
  (if (let-named? exp)
      (cdddr exp)
      (cddr exp)))
(define (let-bindings exp)
  (if (let-named? exp)
      (caddr exp)
      (cadr exp)))
(define (let-vars exp)
  (map car (let-bindings exp)))
;;; This could probably be simplified using a macro, but those haven't been covered in the book up
;;; to this point.
(define (let->combination exp)
  (if (let-named? exp)
      (list 'let
            (let-bindings exp)
            (append (list'define) (list (append (list (let-name exp)) (let-vars exp)))
                  (let-body exp))
            (append (list (let-name exp)) (let-vars exp)))
      (append (list (make-lambda (let-vars exp) (let-body exp)))
              (map cadr (let-bindings exp)))))
