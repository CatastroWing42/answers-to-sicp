#lang sicp

(define (first-denomination kinds-of-coins)
        (cond ((= kinds-of-coins 1) 1)
              ((= kinds-of-coins 2) 5)
              ((= kinds-of-coins 3) 10)
              ((= kinds-of-coins 4) 25)
              ((= kinds-of-coins 5) 50)))
(define (count-change-iter amount n)
  (cond ((< amount 0) 0)
        ((= amount 0) 1)
        ((= n 0) 0)
        (else (+ (count-change-iter amount (- n 1))
                 (count-change-iter (- amount (first-denomination n)) n)))))

(define (count-change amount) (count-change-iter amount 5))

(count-change 1500)