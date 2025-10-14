#lang sicp

(define (iterative-improve improve good-enough?)
  (define (iter guess)
    (if (good-enough? guess) guess
      (iter (improve guess))))
  iter)

(define (square x) (* x x))
(define (average x y) (/ (+ x y) 2))
(define (sqrt x)
  ((iterative-improve (lambda (y) (average y (/ x y)))
                     (lambda (y) (< (abs (- (square y) x)) 0.001))) 1.0))

(sqrt 2)

(define tolerance 0.001)
(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (fixed-point f)
  ((iterative-improve (lambda (x) (f x))
                      (lambda (x) (< (abs (- x (f x))) tolerance))) 2.0))

(fixed-point (lambda (x) (/ (log 1000) (log x))))