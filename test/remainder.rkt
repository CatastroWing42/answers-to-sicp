#lang sicp

(define (f start-time)
    (display (remainder 11 3))
    (newline)
    (display (random 3))
    (newline)
    (display (- (runtime) start-time))
    (newline))

(f (runtime))
(even? 2)
(expt 2 3)
"---------------"
(gcd 206 27)
(abs -12.3)
(log 10)
(- 10 2)
(inc 3)