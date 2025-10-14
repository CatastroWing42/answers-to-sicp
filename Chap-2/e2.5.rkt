#lang sicp

(define (my-cons x y)
  (* (expt 2 x) (expt 3 y)))
(define (my-car z)
    (if (= (remainder z 2) 0)
        (+ 1 (my-car (/ z 2)))
        0))
(define (my-cdr z)
    (if (= (remainder z 3) 0)
        (+ 1 (my-cdr (/ z 3)))
        0))

(my-car (my-cons 7 9))
(my-cdr (my-cons 7 8))