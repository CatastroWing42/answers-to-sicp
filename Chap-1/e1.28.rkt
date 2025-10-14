#lang sicp

(define (square x) (* x x))
(define (miller-rabin-test n)
    (define (non-trivial-root x)
      (if (= x 1) 0 x))
    (define (square-remainder x)
      (cond ((= x 1) 1)
            ((= x (- n 1)) 1)
            (else (non-trivial-root (remainder (square x) n)))))
    (define (expmod base exp)
        (cond ((= exp 0) 1)
              ((even? exp)
               (square-remainder (expmod base (/ exp 2))))
              (else
               (remainder (* base (expmod base (- exp 1))) n))))
    (define (try-it a)
      (cond ((= a n) true)
            ((= (expmod a (- n 1)) 1) (try-it (+ a 1)))
            (else false)))
    (try-it 2))

;; Carmichael numbers
(miller-rabin-test 561)
(miller-rabin-test 1105)
(miller-rabin-test 1729)
(miller-rabin-test 2465)
(miller-rabin-test 2821)
(miller-rabin-test 6601)
"---------------"
;; normal numbers
(miller-rabin-test 6602)
(miller-rabin-test 100)
"---------------"
;; prime numbers
(miller-rabin-test 37)
(miller-rabin-test 23)