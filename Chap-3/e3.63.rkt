#lang sicp

(define (stream-map proc . argstreams)
    (if (stream-null? (car argstreams))
        the-empty-stream
        (cons-stream
            (apply proc (map stream-car argstreams))
            (apply stream-map
                (cons proc (map stream-cdr argstreams))))))

(define (sqrt-stream x)
    (define guesses
        (cons-stream 1.0
            (stream-map (lambda (guess) (sqrt-improve guess x))
                guesses)))
    guesses)

; if not use guesses, substitution model will trigger an (sqrt-stream x) function call
; every time (stream-cdr ) invoked, so its significantly less efficient.

; If we dont use memoized proc, it will just be the same