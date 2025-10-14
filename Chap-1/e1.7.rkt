#lang sicp

#|
small & large numbers use scientific notation
eg. 1.2345e-10 1.2345e+10
0.000000000000000000000128402883000000000000000000383833000001
1.23e-100
|#

(define (abs x) (if (< x 0) (- x) x))
(define (square x) (* x x))
(define expected-delta 0.001)
(define expected-delta-ratio 1e-50)
;;(define (good-enough? guess x)
  ;;(< (abs (- (square guess) x)) expected-delta))
(define (not-zero x y) (if (= x 0) y x))
(define (good-enough? last-guess guess)
  (< (abs (/ (- guess last-guess) (not-zero last-guess guess))) expected-delta-ratio))
(define (average x y) (/ (+ x y) 2))
(define (improve guess x)
  (average guess (/ x guess)))
(define (sqrt-iter last-guess guess x)
        (if (good-enough? guess last-guess)
            guess
            (sqrt-iter guess (improve guess x) x)))
(define (newton-sqrt x) (sqrt-iter 0 1.0 x))

(sqrt 1e-100)
(sqrt 1.224567e-99)
(sqrt 1e+100)
(sqrt 1.2345678e+99)
"-------------------------"
(newton-sqrt 1e-100)
(newton-sqrt 1.224567e-99)
(newton-sqrt 1e+100)
(newton-sqrt 1.2345678e+99)