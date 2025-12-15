#lang sicp

(define (mystery x)
    (define (loop x y)
        (if (null? x)
            y
            (let ((temp (cdr x)))
                (set-cdr! x y)
                (loop temp y))))
    (loop x '()))

(define v (list 'a 'b 'c 'd))
(define w (mystery v))

w
v