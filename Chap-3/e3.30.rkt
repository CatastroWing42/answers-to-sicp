#lang sicp

(define (ripple-carry-adder A B S C)
    (if (null? A)
        (set-signal! C 0)
        (let ((Ai (car A))
                (Bi (car B))
                (Si (car S))
                (Ci (make-wire)))
            (full-adder Ai Bi Ci Si C)
            (ripple-carry-adder
                (cdr A)
                (cdr B)
                (cdr S)
                Ci))))

;delay of n bits will be
; n * (or-delay + 2 * (and-delay + max{or-delay, and-delay + not-delay}))