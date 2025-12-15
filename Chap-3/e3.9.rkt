#lang sicp

(define (factorial n)
    (if (= n 1) 1 (* n (factorial (- n 1)))))
(factorial 5)
; global-frame
; E1(factorial-env bind n:5) -> global-frame
; E2(factorial-env bind n:4) -> global->frame
; ...
; En(factorial-env bind n:1)

(define (factorial n) (fact-iter 1 1 n))
(define (fact-iter product counter max-count)
    (if (> counter max-count)
        product
        (fact-iter (* product counter) (+ counter 1) max-count)))

(factorial 5)
; global-frame
; E1(factorial-env bind n:5) -> global-frame
; find fact-iter in E1->global
; found in global
; E2(fact-iter bind product:1 counter:1 max-count:5) -> global
; E3(fact-iter bind product:1 counter:2 max-count:5) -> global
; ...
; En(fact-iter bind counter:6)