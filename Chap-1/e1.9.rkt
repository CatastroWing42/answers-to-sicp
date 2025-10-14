#lang sicp

(define (inc x) (+ x 1))
(define (dec x) (- x 1))
(define (+ a b)
  (if (= a 0) b (inc (+ (dec a) b))))
  #| this is recursive process (inc (inc (inc ... (dec (dec (dec a..to 0)))))|#
(define (+ a b)
  (if (= a 0) b (+ (dec a) (inc b))))
  #| this is iterative process |#