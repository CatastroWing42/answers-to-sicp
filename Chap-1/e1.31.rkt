#lang sicp

#|
(define (product term a next b)
  (if (> a b) 1
    (* (term a) (product term (next a) next b))))
|#

(define (product term a next b)
  (define (iter a result)
    (if (> a b) result
      (iter (next a) (* result (term a)))))
  (iter a 1))
  

(define (identity x) x)
(define (inc x) (+ x 1))

(define (factorial n)
  (product identity 1 inc n))

(factorial 5)

(define (pi-product n)
  (define (pi-term x)
    (/ (* (- x 1) (+ x 1)) (* x x)))
  (define (pi-next x)
    (+ x 2))
  (* 4 (product pi-term 3.0 pi-next n)))

(pi-product 1000)