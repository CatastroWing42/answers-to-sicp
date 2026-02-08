#lang sicp

(define (memo-proc proc)
    (let ((already-run? false) (run-result false))
        (lambda ()
        (if (not already-run?)
            (begin
                (set! run-result (proc))
                (set! already-run? true)
                run-result)
            run-result))))
(define-syntax delay
    (syntax-rules ()
        ((delay exp) (memo-proc (lambda () exp)))))
; (define (delay exp)
;     (memo-proc
;         (lambda () exp)))
(define (force d) (d))

(define-syntax cons-stream
    (syntax-rules ()
        ((cons-stream a b) (cons a (delay b)))))
; (define (cons-stream a b)
;     (cons a (delay b)))
(define (stream-car s)
    (car s))
(define (stream-cdr s)
    (force (cdr s)))
(define the-empty-stream '())
(define (stream-null? s) (null? s))

(define (stream-ref s n)
    (if (= n 0)
        (stream-car s)
        (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc . argstreams)
    (if (stream-null? (car argstreams))
        the-empty-stream
        (cons-stream
            (apply proc (map stream-car argstreams))
            (apply stream-map
                (cons proc (map stream-cdr argstreams))))))

(define (display-line x)
    (newline) (display x))

(define (show x)
    (display-line x)
    x)

(define (stream-enumerate-interval low high)
    (if (> low high)
        the-empty-stream
        (cons-stream low
            (stream-enumerate-interval (+ low 1) high))))

(display "----------------------")
; 0
(define x
    (stream-map show
        (stream-enumerate-interval 0 10)))
(display "----------------------")
; 1 2 3 4 5 5
(stream-ref x 5)
(display "----------------------")
; 6 7 7
(stream-ref x 7)
(display "----------------------")

(define (stream-filter pred stream)
    (cond
        ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
            (cons-stream (stream-car stream)
                (stream-filter pred (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (stream-for-each proc s)
    (if (not (stream-null? s))
        (begin
            (proc (stream-car s))
            (stream-for-each proc (stream-cdr s)))))
(define (display-stream s)
    (stream-for-each display-line s))

(define sum 0)
(newline)
(display "SUM:")
(display sum)
(newline)
; sum = 0
(define (accum x) (set! sum (+ sum x)) sum)
(newline)
(display "SUM:")
(display sum)
(newline)
; sum = 0
(define seq
    (stream-map accum
        (stream-enumerate-interval 1 20)))
(newline)
(display "SUM:")
(display sum)
(newline)
; sum = 1
(define y (stream-filter even? seq))
(newline)
(display "SUM:")
(display sum)
(newline)
; sum = 6
(define z
    (stream-filter (lambda (x) (= (remainder x 5) 0))
        seq))
(newline)
(display "SUM:")
(display sum)
(newline)
; sum = 10
(stream-ref y 7)
(newline)
(display "SUM:")
(display sum)
(newline)
; sum = 136
; print 136
(display-stream z)
(newline)
(display "SUM:")
(display sum)
(newline)
; sum = 210
; print 10 15 45 55 105 120 190 210

; if dont use memoized proc, sum would be larger, for accum will be called many more times