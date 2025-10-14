#lang sicp

(define (double x) (* 2 x))
(define (halve x) (/ x 2))

(define (multiplication a b)
  (define (iter a b result)
    (cond ((= b 0) result)
          ((even? b) (iter (double a) (halve b) result))
          (else (iter a (- b 1) (+ a result)))))
  (iter a b 0))

(multiplication 2 10)