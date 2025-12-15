#lang sicp

(define (make-accumulator init)
    (lambda (num)
        (begin (set! init (+ init num))
            init)))

(define A (make-accumulator 5))

(A 10)
(A 10)