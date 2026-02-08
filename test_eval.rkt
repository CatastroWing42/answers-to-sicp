#lang sicp

(define (tagged-list? exp tag)
    (if (pair? exp)
        (eq? (car exp) tag)
        false))

(define (make-frame vars vals)
    (cons vars vals))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
    (set-car! frame (cons var (frame-variables frame)))
    (set-cdr! frame (cons val (frame-values frame))))

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (extend-environment vars vals base-env)
    (if (= (length vars) (length vals))
        (cons (make-frame vars vals) base-env)
        (error "mismatch")))

(define (lookup-variable-value var env)
    (define (env-loop env)
        (define (scan vars vals)
            (cond ((null? vars)
                    (env-loop (enclosing-environment env)))
                ((eq? var (car vars))
                    (car vals))
                (else (scan (cdr vars) (cdr vals)))))
        (if (eq? env the-empty-environment)
            (error "unbound" var)
            (let ((frame (first-frame env)))
                (scan (frame-variables frame)
                    (frame-values frame)))))
    (env-loop env))

(define primitive-procedures
    (list
        (list '+ +)
        (list 'car car)))

(define (primitive-procedure-names)
    (map car primitive-procedures))

(define (primitive-procedure-objects)
    (map (lambda (proc) (list 'primitive (cadr proc)))
        primitive-procedures))

(define initial-env
    (extend-environment (primitive-procedure-names)
        (primitive-procedure-objects)
        the-empty-environment))

(define (primitive-procedure? proc)
    (tagged-list? proc 'primitive))

(display "Looking up +")
(newline)
(define val-plus (lookup-variable-value '+ initial-env))
(display "Got: ")
(display val-plus)
(newline)
(display "Is primitive? ")
(display (primitive-procedure? val-plus))
(newline)
