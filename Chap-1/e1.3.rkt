#lang sicp

(define (square x) (* x x))
(define (sum-of-squares x y) (+ (square x) (square y)))
(define (sum-of-squares-of-two-largest x y z)
  (cond ((and (>= x y) (>= z y)) (sum-of-squares x z))
        ((and (>= x z) (>= y z)) (sum-of-squares x y))
        (else (sum-of-squares y z))))

(sum-of-squares-of-two-largest 1 2 3)