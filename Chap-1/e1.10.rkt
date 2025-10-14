#lang sicp

(define (k n) (* 5 n n))
#| (k n): 5n^2 |#

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) (A x (- y 1))))))

(A 1 10) ;; 2^9 * 2 = 1024
#| A(1, n) = 2^n |#
(A 2 4) ;; 65536
#| A(2, n) = (A 1 (A 1...(A 1 2))) [n-1 times "(A 1" part]|#
(A 3 3) ;; (A 2 (A 2 2)) 65536
#| A(3, n) = (A 2 (A 2 (A 2 2))) [n-1 times of "(A 2" part] |#

(define (f n) (A 0 n))
#| f(n) = {n = 0 -> 0; n != 0 -> 2n}|#
(define (g n) (A 1 n))
#| g(n) = {n = 0 -> 0; n != 0 -> 2^n}|#
(define (h n) (A 2 n))
#| h(n) = {n = 0 -> 0; n != 0 -> 2^2^2^2...2^2(n times of 2)|#