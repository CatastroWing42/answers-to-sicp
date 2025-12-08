#lang sicp

;;(define (num_a) (lambda () 2))
;;(put 'a 'b num_a)

(define table (make-hash))
(hash-set! table 'a (lambda () 2))
((hash-ref table 'a))