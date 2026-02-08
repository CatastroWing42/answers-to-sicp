#lang sicp

(define (pair s t)
    (cons-stream
        (list (stream-car s) (stream-car t))
        (interleave
            (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
            (pair (stream-cdr s) (stream-cdr t)))))

; Consider a pair x(m, n), let B(m, n) the pairs beore x
; let P(i) the pairs before x in sub-pairs diagnol starting at (i, i)
; then, P(i) + 2 = 2 * (P(i+1) + 2)
; => P(0) = 2^m * (P(m) + 2) - 2
;
; Now we look at how many pairs before x in sub-pairs diagnol starting at (m, m)
; note that x(m, n) is in same row with (m, m)
; => P(m) = 0; if m == n, 2 * (n - m) - 1; else
; Also note that B(m, n) = P(0)

; #
; B(m, m)
; = 2^(m + 1) - 2; if m == n
; = 2^m * (2 * (n-m) + 1) - 2; else

; B(1, 100) = 396
; B(99, 100) = 2^99 * 3 - 2
; B(100, 100) = 2^101 - 2