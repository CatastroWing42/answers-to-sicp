#lang sicp

(define (double x) (* x 2))
(define (halve x) (/ x 2))

(define (multiplication a b)
  (cond ((= b 0) 0)
        ((even? b) (double (multiplication a (halve b))))
        (else (+ a (multiplication a (- b 1))))))

(multiplication 5 3)