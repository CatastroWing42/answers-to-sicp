#lang sicp

(define (abs x) (if (< x 0) (- x) x))
(define (square x) (* x x))
(define expected-delta 0.001)
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) expected-delta))
(define (average x y) (/ (+ x y) 2))
(define (improve guess x)
  (average guess (/ x guess)))
(define (sqrt-iter guess x)
        (if (good-enough? guess x)
            guess
            (sqrt-iter (improve guess x) x)))
(define (newton-sqrt x) (sqrt-iter 1.0 x))

;;(newton-sqrt 9)
#|
(sqrt 9)
(sqrt 4)
(sqrt (+ 100 37))
(sqrt (+ (sqrt 2) (sqrt 3)))
(square (sqrt 1000))
|#