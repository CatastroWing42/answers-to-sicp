#lang sicp

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

(adjoin-set 9 (list 2 3 6 10 15))
(adjoin-set 1 (list 2 3 6 10 15))
(adjoin-set 20 (list 2 3 6 10 15))
(adjoin-set 3 (list 2 3 6 10 15))