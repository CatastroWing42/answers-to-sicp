#lang sicp

(define (abs x) (if (< x 0) (- x) x))
(define (square x) (* x x))
(define expected-delta-ratio 1e-50)
(define (not-zero x y) (if (= x 0) y x))
(define (good-enough? last-guess guess)
  (< (abs (/ (- guess last-guess) (not-zero last-guess guess))) expected-delta-ratio))
(define (average x y) (/ (+ x y) 2))
(define (improve guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))
(define (cube-root-iter last-guess guess x)
        (if (good-enough? guess last-guess)
            guess
            (cube-root-iter guess (improve guess x) x)))
(define (newton-cube-root x) (cube-root-iter 0 1.0 x))

;;(newton-cube-root 27)

(newton-cube-root 1e-100)
(newton-cube-root 1.224567e-99)
(newton-cube-root 1e+100)
(newton-cube-root 1.2345678e+99)