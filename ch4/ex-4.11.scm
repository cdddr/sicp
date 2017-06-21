;; -*- geiser-scheme-implementation: mit -*-
;;; Represent a frame as a list of bindings where each binding is a name value pair
(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define (make-frame bindings)
  bindings)

(define (add-binding-to-frame! var val frame)
  (set-cdr! frame (cons (car frame) (cdr frame)))
  (set-car! frame (list var val)))

(define (valid-bindings? bindings)
  (reduce-left (lambda (x y) (and x y))
               #t
               (map (lambda (item)
                      (= (length item) 2))
                    bindings)))

(define the-empty-environment '())

(define (extend-environment bindings base-env)
  (display (valid-bindings? bindings))
  (cond ((null? bindings)
         (error "Invalid bindings : the bindings list is null" bindings))
        ((not (valid-bindings? bindings))
         (error "Invalid bindings : there are varaibles without values" bindings))
        (else
         (cons (make-frame bindings) base-env))))

(define (find-binding var env)
  (let ((binding (find-binding-pos var env)))
    (if (null? binding)
        '()
        (car (find-binding-pos var env)))))

(define (find-binding-pos var env)
  (define (env-loop env)
    (define (scan bindings)
      (cond ((null? bindings)
             (env-loop (enclosing-environment env)))
            ((eq? var (car (car bindings)))
             bindings)
            (else (scan (cdr bindings)))))
    (if (eq? env the-empty-environment)
        '()
        (let ((frame (first-frame env)))
          (scan frame))))
  (env-loop env))

(define (lookup-variable-value var env)
  (let ((binding (find-binding var env)))
    (if (null? binding)
        (error "Unbound variable" var)
        (cadr (find-binding var env)))))

(define (set-variable-value! var val env)
  (let ((binding (find-binding var env)))
    (if (null? binding)
        (error "Unbound variable" var)
        (set-car! (cdr (find-binding var env)) val))))

(define (define-variable! var val env)
  (let ((binding (find-binding var env)))
    (if (null? binding)
        (add-binding-to-frame! var val (first-frame env))
        (set-car! (cdr binding) val))))
