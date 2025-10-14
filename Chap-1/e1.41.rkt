#lang sicp

(define (double f)
  (lambda (x) (f (f x))))

((double inc) 2)

(((double (double double)) inc) 5)
;; (((double (lambda (x) (double (double x)))) inc) 5)
;; (((lambda (x) (double (double (double (double x))))) inc) 5)
;; 2^4 inc => 21