#lang sicp

(define (square x) (* x x))
(define (fermat-test-full n)
    (define (expmod base exp m)
        (cond ((= exp 0) 1)
              ((even? exp)
               (remainder (square (expmod base (/ exp 2) m)) m))
              (else
               (remainder (* base (expmod base (- exp 1) m)) m))))
    (define (try-it a)
      (cond ((= a n) true)
            ((= (expmod a n n) a) (try-it (+ a 1)))
            (else false)))
    (try-it 2))

;; Carmichael numbers
(fermat-test-full 561)
(fermat-test-full 1105)
(fermat-test-full 1729)
(fermat-test-full 2465)
(fermat-test-full 2821)
(fermat-test-full 6601)
"---------------"
;; normal numbers
(fermat-test-full 6602)
(fermat-test-full 100)
"---------------"
;; prime numbers
(fermat-test-full 37)
(fermat-test-full 23)