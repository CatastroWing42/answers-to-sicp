#lang sicp

(define (make-f y)
    (lambda (x)
        (begin (set! y (+ y x)) y)))
(define f 
    (make-f -0.5))

(f 0)
(f 1)