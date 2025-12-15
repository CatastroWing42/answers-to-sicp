#lang sicp

(define (rand-update x)
    (let ((a 2)
            (b 11)
            (m 7))
        (remainder (+ (* a x) b)
            m)))

(define x 0)
(define (rand act)
    (define reset
        (lambda (new-value)
            (set! x new-value)))
    (cond ((eq? act 'generate)
            (begin (set! x (rand-update x))
                x))
        ((eq? act 'reset) reset)
        (else "Invalid action")))

(rand 'generate)
(rand 'generate)
(rand 'generate)
(rand 'generate)
((rand 'reset) 2)
(rand 'generate)
(rand 'generate)
(rand 'generate)
(rand 'generate)