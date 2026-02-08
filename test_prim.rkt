#lang sicp

(define (tagged-list? exp tag)
    (if (pair? exp)
        (eq? (car exp) tag)
        false))

(define primitive-procedures
    (list
        (list 'car car)
        (list '+ +)))

(define (primitive-procedure-objects)
    (map (lambda (proc) (list 'primitive (cadr proc)))
        primitive-procedures))

(define prims (primitive-procedure-objects))
(display "Primitive objects: ")
(display prims)
(newline)

(define (primitive-procedure? proc)
    (tagged-list? proc 'primitive))

(display "Test 1: ")
(display (primitive-procedure? (car prims)))
(newline)

(display "Car prim: ")
(display (car prims))
(newline)
