#lang sicp

(define (unless condition usual-value exception-value)
    (if condition exception-value usual-value))

(define (fact n)
    (unless (= n 1)
        (* n (fact (- n 1)))
        1))

(fact 5)