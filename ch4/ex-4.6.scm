;;;  -*- geiser-scheme-implementation: mit -*-
;;; Let expressions are derived expressions because
;; (let ((<var1> <exp1>)
;;       (<var2> <exp2>)
;;       (...)
;;       (<varn> <expn>))
;;   <body>)

;; is equivalent to

;; ((lambda (<var1> ... <varn>) <body) <exp1> ... <expn>)
(define (let? exp) (tagged-list? exp 'let))
(define (let-body exp) (cddr exp))
(define (let-bindings exp) (cadr exp))
(define (let->combination exp)
  (append (list (make-lambda (map car (let-bindings exp)) (let-body exp)))
          (map cadr (let-bindings exp))))


;; 1 (user) => (let->combination '(let ((a 1) (b 2) (c 3)) (+ a b c)))

;; ;Value 67: ((lambda (a b c) (+ a b c)) 1 2 3)
