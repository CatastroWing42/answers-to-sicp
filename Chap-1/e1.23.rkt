#lang sicp

(define (next n)
  (if (= n 2) 3 ( + n 2)))
(define (square x) (* x x))
(define (smallest-divisor n)
  (define (divides? a b)
    (= (remainder b a) 0))
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
            ((divides? test-divisor n) test-divisor)
            (else (find-divisor n (next test-divisor)))))
  (find-divisor n 2))

(define (prime? n)
  (= n (smallest-divisor n)))


(define (search-for-primes start number)
    (define (timed-prime-test n)
        (newline)
        (display n)
        (start-prime-test n (runtime)))
    (define (start-prime-test n start-time)
        (if (prime? n) (report (- (runtime) start-time))))
    (define (report elapsed-time)
        (display " *** ")
        (display elapsed-time))
  (cond ((even? start) (search-for-primes (+ start 1) number))
        ((= number 0) (newline))
        (else (timed-prime-test start)
              (search-for-primes (+ start 2) (- number 1)))))

;; (timed-prime-test 1009)
(search-for-primes 1000 20)
"---------------"
(search-for-primes 10000 20)
"---------------"
(search-for-primes 100000 20)
"---------------"
(search-for-primes 1000000 20)