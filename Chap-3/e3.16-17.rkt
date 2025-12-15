#lang sicp

(define (count-pairs x)
    (if (not (pair? x))
        0
        (+ (count-pairs (car x))
            (count-pairs (cdr x))
            1)))

(define (count-distinct-pairs x)
    (define counted-list '())
    (define (in-counted? x list)
        (cond ((null? list) false)
            ((eq? x (car list)) true)
            (else (in-counted? x (cdr list)))))
    (define (in-counted-list? x)
        (in-counted? x counted-list))
    (define (add-list x)
        (set! counted-list (cons x counted-list)))
    (define (count x)
        (cond ((not (pair? x)) 0)
            ((in-counted-list? x) 0)
            (else (begin
                    (add-list x)
                    (+ (count (car x))
                        (count (cdr x))
                        1)))))
    (count x))

(define a (cons 'a 'b))
(define b (cons 'c 'd))
(define c (cons 'e 'f))

(define x3 (cons b c))
(count-pairs x3)
(count-distinct-pairs x3)

(define x4 (cons 'a (cons c c)))
(count-distinct-pairs x4)
(count-pairs x4)

(define b7 (cons c c))
(define x7 (cons b7 b7))
(count-distinct-pairs x7)
(count-pairs x7)

(define cn (cons 'e 'f))
(set-cdr! cn cn)
(define xn (cons 'a (cons 'b cn)))
(count-distinct-pairs xn)
(count-pairs xn)