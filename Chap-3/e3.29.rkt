#lang sicp

(define (or-gate o1 o2 output)
     (let ((n1 (make-wire))
            (n2 (make-wire))
            (a (make-wire)))
        (not-gate o1 n1)
        (not-gate o2 n2)
        (and-gate n1 n2 a)
        (not-gate a output)
        'ok))

; this or-gate-delay will be 2 * not-gate-delay + and-gate-delay