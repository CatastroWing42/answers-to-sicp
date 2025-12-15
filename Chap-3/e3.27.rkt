#lang sicp

; O(n) time because last two fib(n) result will in front of table
; so it will be n * 2

; it will not work if just 
(define (mem-fib n)
    (memoize fib))
; table will only used once for inarg `n