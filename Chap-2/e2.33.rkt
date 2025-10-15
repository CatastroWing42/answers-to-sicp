#lang sicp

(define (accumulate op initial sequence)
  (if (null? sequence) initial
    (op (car sequence) (accumulate op initial (cdr sequence)))))

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))
(define (append seq1 seq2)
  (accumulate cons seq2 seq1))
(define (my-length sequence)
  (accumulate (lambda (x y) (inc y)) 0 sequence))

(define (square x) (* x x))
(append (list 1 2 3) (list 4 5 6))
(map square (list 1 2 3))
(my-length (list 1 2 3 4))