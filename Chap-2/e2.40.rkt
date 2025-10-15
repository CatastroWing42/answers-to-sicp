#lang sicp

(define (accumulate op initial sequence)
  (if (null? sequence) initial
    (op (car sequence) (accumulate op initial (cdr sequence)))))

(define (flatmap proc s)
  (accumulate append nil (map proc s)))

(define (enumerate-number low high)
  (if (> low high) nil
    (cons low (enumerate-number (+ low 1) high))))
(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
               (enumerate-number 1 (- i 1)))) (enumerate-number 1 n)))

(unique-pairs 4)

;; following code not runnable, no prime? definition
(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))
(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))
(define (prime-sum-pairs n)
  (map make-pair-sum
    (filter prime-sum? (unique-pairs n))))