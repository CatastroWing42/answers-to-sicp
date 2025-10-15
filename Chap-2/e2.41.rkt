#lang sicp

(define (triple-0 t)
  (car t))
(define (triple-1 t)
  (cadr t))
(define (triple-2 t)
  (caddr t))

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
(define (unique-triples n)
  (flatmap (lambda (i)
             (map (lambda (p) (append (list i) p))
               (unique-pairs (- i 1))))
            (enumerate-number 1 n)))

;; (unique-triples 7)

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence)) (cons (car sequence) (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (s-sum-triples n s)
  (filter (lambda (triple)
            (= s (+ (triple-0 triple) (triple-1 triple) (triple-2 triple))))
            (unique-triples n)))

(s-sum-triples 7 10)