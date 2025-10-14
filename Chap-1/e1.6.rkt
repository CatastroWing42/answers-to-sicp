#lang sicp
(#%require "sqrt-newton.rkt")
(define (new-if predicate consequent alternative)
    (cond (predicate consequent)
          (else alternative)))

(define (new-sqrt-iter guess x)
        (new-if (good-enough? guess x)
            guess
            (sqrt-iter (improve guess x) x)))
(define (new-sqrt x) (new-sqrt-iter 1.0 x))
"----------------"
(new-sqrt 9) ;;this will hang
#|
new-if is not a special form, so its evaluation process
needs to evaluate all operands first(interpretor uses application-order).
|#