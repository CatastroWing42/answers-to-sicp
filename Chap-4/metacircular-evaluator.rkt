#lang sicp

(define (tagged-list? exp tag)
    (if (pair? exp)
        (eq? (car exp) tag)
        false))

(define (eval exp env)
    (cond
        ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp) (make-procedure (lambda-parameters exp)
                                        (lambda-body exp)
                                        env))
        ((begin? exp)
            (eval-sequence (begin-actions exp) env))
        ((cond? exp)
            (eval (cond->if exp) env))
        ((and? exp) (eval-and exp env))
        ((or? exp) (eval-or exp env))
        ((let? exp) (eval (let->combination exp) env))
        ((let*? exp) (eval (let*->nested-let exp) env))
        ((unbound? exp) (eval-unbound exp env))
        ((application? exp)
            (my-apply (eval (operator exp) env)
                    (list-of-values (operands exp) env)))
        (else (error "Unknown expression type: EVAL" exp))))

(define (my-apply procedure arguments)
    (cond ((primitive-procedure? procedure)
            (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
            (eval-sequence
                (procedure-body procedure)
                (extend-environment
                    (procedure-parameters procedure)
                    arguments
                    (procedure-environment procedure))))
        (else (error "Unknown procedure type: APPLY" procedure))))

(define (list-of-values exp env)
    (if (no-operands? exp)
        '()
        (cons (eval (first-operand exp) env)
            (list-of-values (rest-operands exp) env))))

(define (eval-if exp env)
    (if (true? (eval (if-predicate exp) env))
        (eval (if-consequent exp) env)
        (eval (if-alternative exp) env)))

(define (eval-sequence exp env)
    (cond ((last-exp? exp)
            (eval (first-exp exp) env))
        (else
            (eval (first-exp exp) env)
            (eval-sequence (rest-exps exp) env))))

(define (eval-assignment exp env)
    (set-variable-value! (assignment-variable exp)
                        (eval (assignment-value exp) env)
                        env)
    'ok)

(define (eval-definition exp env)
    (define-variable! (definition-variable exp)
                        (eval (definition-value exp) env)
                        env)
    'ok)

; 4.1
; from left to right
(define (list-of-values-ltr exp env)
    (if (no-operands? exp)
        '()
        (let ((first-value (eval (first-operand exp) env)))
            (cons first-value
                (list-of-values-ltr (rest-operands exp) env)))))
; from right to left
(define (list-of-values-rtl exp env)
    (if (no-operands? exp)
        '()
        (let ((rest-values (list-of-values-rtl (rest-operands exp) env)))
            (cons (eval (first-operand exp) env) rest-values))))

(define (self-evaluating? exp)
    (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))

; assignment has the form (set! <variable> <value>)
(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

; definition has the form
; (define <var> <value>)
; (define (<var> <parameter1> ... <parametern>)
;   <body>)
(define (definition? exp) (tagged-list? exp 'define))
(define (definition-variable exp)
    (if (symbol? (cadr exp))
        (cadr exp)
        (caadr exp)))
(define (definition-value exp)
    (if (symbol? (cadr exp))
        (caddr exp)
        (make-lambda (cdadr exp)
                (cddr exp))))

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
    (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
    (if (not (null? (cdddr exp)))
        (cadddr exp)
        'false))

(define (make-if predicate consequent alternative)
    (list 'if predicate consequent alternative))

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
    (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause clause)
    (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp) (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
    (if (null? clauses)
        'false
        (let ((first (car clauses))
                (rest (cdr clauses)))
            (if (cond-else-clause first)
                (if (null? rest)
                    (sequence->exp (cond-actions first))
                    (error "else not the last clause"))
                (make-if (cond-predicate first)
                    (sequence->exp (cond-actions first))
                    (expand-clauses rest))))))

; 4.2
; a. application only checks whether its a pair,
;    define will be parsed as an application.
; b
(define (call-application? exp) (tagged-list? exp 'call))
(define (call-operator exp) (cadr exp))
(define (call-operands exp) (cddr exp))

; 4.3 - 注释掉以避免 put/get 的依赖
; (define (install-quoted-package)
;     (define (pack-eval-quoted exp env)
;         (text-of-quotation exp))
;     (put 'eval 'quote pack-eval-quoted))
; (install-quoted-package)
;
; (define (install-assignment-package)
;     (define (pack-eval-assignment exp env)
;         (eval-assignment exp env))
;     (put 'eval 'assignment pack-eval-assignment))
; (install-assignment-package)
;
; (define (install-definition-package)
;     (define (pack-eval-definition exp env)
;         (eval-definition exp env))
;     (put 'eval 'define pack-eval-definition))
; (install-definition-package)
;
; (define (install-if-package)
;     (define (pack-eval-if exp env)
;         (eval-if exp env))
;     (put 'eval 'if pack-eval-if))
; (install-if-package)
;
; (define (install-lambda-package)
;     (define (pack-eval-lambda exp env)
;         (make-procedure (lambda-parameters exp)
;                     (lambda-body exp)
;                     env))
;     (put 'eval 'lambda pack-eval-lambda))
; (install-lambda-package)
;
; (define (install-begin-package)
;     (define (pack-eval-begin exp env)
;         (eval-sequence (begin-actions exp) env))
;     (put 'eval 'begin pack-eval-begin))
; (install-begin-package)
;
; (define (install-cond-package)
;     (define (pack-eval-cond exp env)
;         (eval (cond->if exp) env))
;     (put 'eval 'cond pack-eval-cond))
; (install-cond-package)
;
; (define (install-application-package)
;     (define (pack-apply exp env)
;         (apply (eval (operator exp) env)
;             (list-of-values (operands exp) env)))
;     (put 'eval 'call pack-apply))
; (install-application-package)

; (define (data-directed-eval exp env)
;     (let ((key (car exp)))
;         (let ((f (get 'eval key)))
;             (if f (f exp env)
;                 (cond ((self-evaluating? exp) exp)
;                     ((variable? exp) (lookup-variable-value exp env))
;                     (else (error "Unknown expression type: EVAL" exp)))))))

; 4.4
(define (and? exp) (tagged-list? exp 'and))
(define (and-actions exp) (cdr exp))

(define (eval-and-seq seq env)
    (cond ((null? seq) true)
        ((last-exp? seq) (eval (first-exp seq) env))
        (else (let ((first-result (eval (first-exp seq) env)))
            (if first-result (eval-and-seq (rest-exps seq) env)
                false)))))
(define (eval-and exp env)
    (eval-and-seq (and-actions exp) env))

(define (or? exp) (tagged-list? exp 'or))
(define (or-actions exp) (cdr exp))

(define (eval-or-seq seq env)
    (if (null? seq) false
        (if (eval (first-exp seq) env) true
            (eval-or-seq (rest-exps seq) env))))
(define (eval-or exp env)
    (eval-or-seq (or-actions exp) env))

; implement and/or as derived expressions
(define (and->if exp) (expand-and-actions (and-actions exp)))
(define (expand-and-actions actions)
    (if (null? actions)
        'true
        (let ((first (car actions))
                (rest (cdr actions)))
            (if (null? rest)
                first
                (make-if first
                    (expand-and-actions rest)
                    'false)))))

(define (or->if exp) (expand-or-actions (or-actions exp)))
(define (expand-or-actions actions)
    (if (null? actions)
        'false
        (let ((first (car actions))
                (rest (cdr actions)))
            (make-if first
                'true
                (expand-or-actions rest)))))

; 4.5
(define (assoc key table-list)
    (cond ((null? table-list) false)
        ((eq? key (caar table-list)) (car table-list))
        (else (assoc key (cdr table-list)))))

(define (cond-recipient-clause clause)
    (eq? (cadr clause) '=>))
(define (cond-recipient-actions clause)
    (cddr clause))
(define (cond->if-recipient exp)
    (expand-clauses-recipient (cond-clauses exp)))
(define (expand-clauses-recipient clauses)
    (if (null? clauses)
        'false
        (let ((first (car clauses))
                (rest (cdr clauses)))
            (if (cond-else-clause first)
                (if (null? rest)
                    (sequence->exp (cond-actions first))
                    (error "else not the last clause"))
                (make-if (cond-predicate first)
                    (make-application (sequence->exp (cond-recipient-actions first))
                        (cond-predicate first))
                    (expand-clauses-recipient rest))))))

(define (make-application procedure parameters)
    (cons procedure parameters))

; 4.6
(define (map f l)
    (if (null? l)
        '()
        (cons (f (car l)) (map f (cdr l)))))

(define (let? exp) (tagged-list? exp 'let))
(define (let-bindings exp) (cadr exp))
(define (let-body exp) (caddr exp))
(define (let-vars exp) (map car (let-bindings exp)))
(define (let-exps exp) (map cadr (let-bindings exp)))
(define (let->combination exp)
    (make-application (make-lambda (let-vars exp) (let-body exp))
        (let-exps exp)))

; 4.7
(define (make-let bindings body)
    (list 'let bindings body))

(define (let*? exp) (tagged-list? exp 'let*))
(define (let*-bindings exp) (cadr exp))
(define (let*-body exp) (caddr exp))
(define (let*->nested-let exp) (nest-let (let*-bindings exp) (let*-body exp)))
(define (nest-let bindings body)
    (if (null? bindings)
        body
        (make-let (car bindings)
            (nest-let (cdr bindings) body))))
; yes, its sufficient to add a clause to eval whose action is
; (eval (let*->nested-let exp) env)

; 4.8
; (define (let-name exp) (cadr exp))
; (define (let-bindings-named exp) (caddr exp))
; (define (let-body exp) (cadddr exp))
; (define (let-vars exp) (map car (let-bindings exp)))
; (define (let-exps exp) (map cadr (let-bindings exp)))
; (define (let->combination-named exp)
;     (make-application (make-lambda
;                         (cons (let-name exp) (let-vars exp))
;                         (let-body exp))
;         (cons (let-body exp) (let-exps exp))))

(define (true? exp) (not (eq? exp false)))
(define (false? exp) (eq? exp false))

(define (make-procedure parameters body env)
    (list 'procedure parameters
        (scan-out-defines body) env))
(define (compound-procedure? p)
    (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame vars vals)
    (cons vars vals))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
    (set-car! frame (cons var (frame-variables frame)))
    (set-cdr! frame (cons val (frame-values frame))))
(define (set-frame-variables! frame vars)
    (set-car! frame vars))
(define (set-frame-values! frame vals)
    (set-cdr! frame vals))

(define (extend-environment vars vals base-env)
    (if (= (length vars) (length vals))
        (cons (make-frame vars vals) base-env)
        (error "variable and value number mismatch")))
(define (lookup-variable-value var env)
    (define (env-loop env)
        (define (scan vars vals)
            (cond ((null? vars)
                    (env-loop (enclosing-environment env)))
                ((eq? var (car vars))
                    (let ((value (car vals))) ; 4.16
                        (if (eq? value '*unassigned*)
                            (error "Eval unassigned variable" var)
                            value)))
                (else (scan (cdr vars) (cdr vals)))))
        (if (eq? env the-empty-environment)
            (error "Unbound variable" var)
            (let ((frame (first-frame env)))
                (scan (frame-variables frame)
                    (frame-values frame)))))
    (env-loop env))
(define (set-variable-value! var val env)
    (define (env-loop env)
        (define (scan vars vals)
            (cond ((null? vars)
                    (env-loop (enclosing-environment env)))
                ((eq? var (car vars))
                    (set-car! vals val))
                (else (scan (cdr vars) (cdr vals)))))
        (if (eq? env the-empty-environment)
            (error "Unbound variable" var)
            (let ((frame (first-frame env)))
                (scan (frame-variables frame)
                    (frame-values frame)))))
    (env-loop env))
(define (define-variable! var val env)
    (let ((frame (first-frame env)))
        (define (scan vars vals)
            (cond ((null? vars)
                    (add-binding-to-frame! var val frame))
                ((eq? var (car vars))
                    (set-car! vals val))
                (else (scan (cdr vars) (cdr vals)))))
        (scan (frame-variables frame) (frame-values frame))))

; 4.11
; (define (make-frame vars vals)
;     (if (null? vars)
;         '()
;         (cons (cons (car vars) (car vals))
;             (make-frame (cdr vars) (cdr vals)))))
; (define (frame-variables frame)
;     (map car frame))
; (define (frame-values frame)
;     (map cdr frame))
; (define (add-binding-to-frame! var val frame)
;     (cons (cons var val) frame))

; 4.12
; (define (env-loop-generic env ops)
;     (define (scan vars vals)
;         (cond ((null? vars)
;                 ((ops 'null) env))
;             ((eq? var (car vars))
;                 ((ops 'eq) vals))
;             (else (scan (cdr vars) (cdr vals)))))
;     (if (eq? env the-empty-environment)
;         (error "Unbound variable" var)
;         (let ((frame (first-frame env)))
;             (scan (frame-variables frame)
;                 (frame-values frame)))))
;
; (define (lookup-variable-value var env)
;     (define (null-ops env)
;         (env-loop-generic (enclosing-environment env) ops))
;     (define (eq-ops vals) (car vals))
;     (define (ops m)
;         (cond ((eq? m 'null) null-ops)
;             ((eq? m 'eq) eq-ops)
;             (else (error "Unknown operation" m))))
;     (env-loop-generic env ops))
; (define (set-variable-value! var val env)
;     (define (null-ops env)
;         (env-loop-generic (enclosing-environment env) ops))
;     (define (eq-ops vals)
;         (set-car! vals val))
;     (define (ops m)
;         (cond ((eq? m 'null) null-ops)
;             ((eq? m 'eq) eq-ops)
;             (else (error "Unknown operation" m))))
;     (env-loop-generic env ops))
; (define (define-variable! var val env)
;     (define (null-ops env)
;         (add-binding-to-frame! var val (first-frame env)))
;     (define (eq-ops vals)
;         (set-car! vals val))
;     (define (ops m)
;         (cond ((eq? m 'null) null-ops)
;             ((eq? m 'eq) eq-ops)
;             (else (error "Unknown operation" m))))
;     (env-loop-generic env ops))

; 4.13
; Specification: Only undefine binding that of the first-frame
(define (make-unbound! var)
    (list 'unbound var))
(define (unbound-variable exp) (cadr exp))

(define (unbound? exp) (tagged-list? exp 'unbound))
(define (eval-unbound exp env)
    (undefine-variable!
        (unbound-variable exp)
        env))

(define (undefine-variable! var env)
    (let ((frame (first-frame env)))
        (define (scan vars vals)
            (cond ((null? vars) '())
                ((eq? var (car vars))
                    (scan (cdr vars) (cdr vals)))
                (else (cons (cons (car vars) (car vals))
                        (scan (cdr vars) (cdr vals))))))
        (let ((vvl (scan (frame-variables frame) (frame-values frame))))
            (set-frame-variables! frame
                (map car vvl))
            (set-frame-values! frame
                (map cdr vvl)))))

(define (primitive-procedure? proc)
    (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
    (list
        (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '= =)
        (list '< <)
        (list '> >)
        (list '<= <=)
        (list '>= >=)
        (list 'list list)
        (list 'number? number?)
        (list 'symbol? symbol?)
        (list 'pair? pair?)
        (list 'eq? eq?)
        (list 'display display)
        (list 'newline newline)
        (list 'not not)))
(define (primitive-procedure-names)
    (map car primitive-procedures))
(define (primitive-procedure-objects)
    (map (lambda (proc) (list 'primitive (cadr proc)))
        primitive-procedures))

(define (setup-environment)
    (let ((initial-env
            (extend-environment (primitive-procedure-names)
                (primitive-procedure-objects)
                the-empty-environment)))
        (define-variable! 'true true initial-env)
        (define-variable! 'false false initial-env)
        initial-env))

(define the-global-environment (setup-environment))

(define (apply-primitive-procedure proc args)
    (apply (primitive-implementation proc) args))

(define input-prompt ";;; Beard-Fish input:")
(define output-prompt ";;; Beard-Fish output:")
(define (driver-loop)
    (prompt-for-input input-prompt)
    (let ((input (read)))
        (let ((output (eval input the-global-environment)))
            (announce-output output-prompt)
            (user-print output)))
    (driver-loop))
(define (prompt-for-input string)
    (newline) (newline) (display string) (newline))
(define (announce-output string)
    (newline) (display string) (newline))
(define (user-print object)
    (if (compound-procedure? object)
        (display (list 'compound-procedure
                    (procedure-parameters object)
                    (procedure-body object)
                    '<procedure-env>))
        (display object)))

; And here's the run!
(driver-loop)

; 4.15
; if (halt try try) returns true, (try try) will run forever
; if (halt try try) returns false, (try try) will return 'halt
; both contradictory

; 4.16
(define (filter f seq)
    (cond
        ((null? seq) '())
        ((f (car seq)) (cons (car seq) (filter f (cdr seq))))
        (else (filter f (cdr seq)))))
(define (append-list a b)
    (if (null? a) b
        (cons (car a) (append-list (cdr a) b))))

(define (definition->assignment def)
    (set-car! def 'set!))

(define (scan-out-defines exp)
    (let ((definitions (filter (lambda (e) (definition? e)) exp)))
        (if definitions
            (let ((bindings
                    (map (lambda (def) (list (definition-variable def) '*unassigned*))
                        definitions))
                    (assignment-exps
                        (map definition->assignment definitions))
                    (rest-exps
                        (filter (lambda (e) (not (definition? e))) exp)))
                (make-let bindings
                    (append-list assignment-exps rest-exps)))
            exp)))

; install in make-procedure, so scan-out-defines only executes once

; 4.17
; An alternative way with no additional frame is to reorder the
; expression sequence, putting definitions before any evaluations.
(define (simultaneous-reorder exp)
    (let ((definitions (filter (lambda (e) (definition? e)) exp)))
        (if definitions
            (let ((rest
                    (filter (lambda (e) (not (definition? e))) exp)))
                (append-list definitions rest))
            exp)))

; 4.20
(define (letrec? exp) (tagged-list? exp 'letrec))
(define (letrec-bindings exp) (cadr exp))
(define (letrec-body exp) (cddr exp))
(define (letrec-variables exp) (map car (letrec-bindings exp)))
(define (letrec-values exp) (map cadr (letrec-bindings exp)))

(define (letrec->let exp)
    (make-let (map (lambda (var) (list var '*unassigned*))
                    (letrec-variables exp))
        (append-list
            (map (lambda (binding) (cons 'set binding))
                (letrec-bindings exp))
            (letrec-body exp))))
; let cannot use recursion variables defined in other bindings

(define (analyze-eval exp env) ((analyze exp) env))
(define (analyze exp)
    (cond
        ((self-evaluating? exp) (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((let? exp) (analyze (let->combination exp))) ; 4.22
        ((application? exp) (analyze-application exp))
        (else (error "Unknown expression type: ANALYZE" exp))))

(define (analyze-self-evaluating exp)
    (lambda (env) exp))
(define (analyze-quoted exp)
    (let ((quoted (text-of-quotation exp)))
        (lambda (env) quoted)))
(define (analyze-variable exp)
    (lambda (env) (lookup-variable-value exp env)))

(define (analyze-assignment exp)
    (let ((var (assignment-variable exp))
            (vproc (analyze (assignment-value exp))))
        (lambda (env)
            (set-variable-value! var (vproc env) env))))
(define (analyze-definition exp)
    (let ((var (definition-variable exp))
            (vproc (analyze (definition-value exp))))
        (lambda (env)
            (define-variable! var (vproc env) env))))

(define (analyze-if exp)
    (let ((pproc (analyze (if-predicate exp)))
            (cproc (analyze (if-consequent exp)))
            (aproc (analyze (if-alternative exp))))
        (lambda (env)
            (if (true? (pproc env))
                (cproc env)
                (aproc env)))))

(define (analyze-lambda exp)
    (let ((vars (lambda-parameters exp))
            (bproc (analyze-sequence (lambda-body exp))))
        (lambda (env)
            (make-procedure vars bproc env))))

(define (analyze-sequence exps)
    (define (sequentially proc1 proc2)
        (lambda (env) (proc1 env) (proc2 env)))
    (define (loop first-proc rest-procs)
        (if (null? rest-procs)
            first-proc
            (loop (sequentially first-proc (car rest-procs))
                (cdr rest-procs))))
    (let ((procs (map analyze exps)))
        (if (null? procs)
            (error "Empty sequence: ANALYZE")
            (loop (car procs) (cdr procs)))))

(define (analyze-application exp)
    (let ((fproc (analyze (operator exp)))
            (aprocs (map analyze (operands exp))))
        (lambda (env)
            (execute-application
                (fproc env)
                (map (lambda (aproc) (aproc env))
                    aprocs)))))
(define (execute-application proc args)
    (cond
        ((primitive-procedure? proc)
            (apply-primitive-procedure proc args))
        ((compound-procedure? proc)
            ((procedure-body proc)
                (extend-environment
                    (procedure-parameters proc)
                    args
                    (procedure-environment proc))))
        (else (error "Unknown procedure type: EXECUTE-APPLICATION" proc))))