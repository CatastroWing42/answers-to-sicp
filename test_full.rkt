#lang sicp

(define (tagged-list? exp tag)
    (if (pair? exp)
        (eq? (car exp) tag)
        false))

(define (self-evaluating? exp)
    (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (make-frame vars vals)
    (cons vars vals))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (extend-environment vars vals base-env)
    (cons (make-frame vars vals) base-env))

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

(define (primitive-procedure? proc)
    (tagged-list? proc 'primitive))

(define (compound-procedure? p)
    (tagged-list? p 'procedure))

(define (list-of-values exps env)
    (if (no-operands? exps)
        '()
        (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (apply procedure arguments)
    (display "apply called with: ")
    (display procedure)
    (display " and ")
    (display arguments)
    (newline)
    (cond ((primitive-procedure? procedure)
            (display "  -> primitive")
            (newline)
            (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
            (display "  -> compound")
            (newline)
            'ok)
        (else 
            (display "  -> unknown type!")
            (newline)
            (error "Unknown procedure type: APPLY" procedure))))

(define (apply-primitive-procedure proc args)
    (apply-in-underlying-scheme
        (cadr proc) args))

(define (apply-in-underlying-scheme proc args)
    (apply proc args))

(define (eval exp env)
    (cond ((self-evaluating? exp) 
            (display "self-eval: ")
            (display exp)
            (newline)
            exp)
        ((variable? exp) 
            (display "var lookup: ")
            (display exp)
            (newline)
            (lookup-variable-value exp env))
        ((application? exp)
            (display "application: ")
            (display exp)
            (newline)
            (apply (eval (operator exp) env)
                    (list-of-values (operands exp) env)))
        (else (error "Unknown expression type: EVAL" exp))))

(define primitive-procedures
    (list
        (list '+ +)))

(define (primitive-procedure-names)
    (map car primitive-procedures))

(define (primitive-procedure-objects)
    (map (lambda (proc) (list 'primitive (cadr proc)))
        primitive-procedures))

(define initial-env
    (extend-environment (primitive-procedure-names)
        (primitive-procedure-objects)
        the-empty-environment))

(display "Testing (+ 2 3)")
(newline)
(display (eval '(+ 2 3) initial-env))
(newline)
