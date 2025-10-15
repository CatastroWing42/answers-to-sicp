#lang sicp

(define (same-parity a . l)
  (define (psame? x)
    (even? (- a x)))
  (define (iter items result)
    (cond ((null? items) result)
          ((psame? (car items)) (cons (car items) (iter (cdr items) result)))
          (else (iter (cdr items) result))))
  (cons a (iter l nil)))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)