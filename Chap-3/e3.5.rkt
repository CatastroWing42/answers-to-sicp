#lang sicp

(define (monte-carlo trials experiment)
    (define (iter trials-remaining trials-passed)
        (cond ((= trials-remaining 0)
                (/ trials-passed trials))
            ((experiment)
                (iter (- trials-remaining 1)
                    (+ trials-passed 1)))
            (else
                (iter (- trials-remaining 1)
                    trials-passed))))
    (iter trials 0))

(define (random-in-range low high)
    (if (> low high)
        (random-in-range high low)
        (let ((range (- high low)))
            (+ low (random range)))))
(define (integral-test P x1 x2 y1 y2)
    (lambda ()
        (P (random-in-range x1 x2) (random-in-range y1 y2))))
(define (estimate-integral P x1 x2 y1 y2 trials)
    (let ((square-area (* (abs (- x1 x2))
                            (abs (- y1 y2))))
            (hit-ratio (monte-carlo trials (integral-test P x1 x2 y1 y2))))
        (* square-area hit-ratio)))

(define (square x) (* x x))
;(define P
;    (lambda (x y)
;        (<= (+ (square (- x 5))
;                (square (- y 7)))
;            (square 3))))
;
;(estimate-integral P 2.0 8.0 4.0 10.0 10000)

(define P1
    (lambda (x y)
        (<= (+ (square (- x 1))
                (square (- y 1)))
            (square 1))))
(estimate-integral P1 0.0 2.0 0.0 2.0 10000000)