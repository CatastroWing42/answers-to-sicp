#lang sicp

(define (fermat-test n)
    #|
    (define (expmod base exp m)
        (cond ((= exp 0) 1)
              ((even? exp)
               (remainder (square (expmod base (/ exp 2) m)) m))
              (else
               (remainder (* base (expmod base (- exp 1) m)) m))))
    |#
    (define (expmod base exp m)
        (cond ((= exp 0) 1)
              ((even? exp)
               (remainder (* (expmod base (/ exp 2) m) ;; this computes twice in each recursion
                             (expmod base (/ exp 2) m)) m))
              (else
               (remainder (* base (expmod base (- exp 1) m)) m))))
    (define (try-it a)
      (= (expmod a n n) a))
    (try-it (+ 1 (random (- n 1)))))
(define (fast-prime? n times)
    (cond ((= times 0) true)
          ((fermat-test n) (fast-prime? n (- times 1)))
          (else false)))


(define (search-for-primes start number)
    (define (timed-prime-test n)
        (newline)
        (display n)
        (start-prime-test n (runtime)))
    (define (start-prime-test n start-time)
        (if (fast-prime? n 5) (report (- (runtime) start-time))))
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
