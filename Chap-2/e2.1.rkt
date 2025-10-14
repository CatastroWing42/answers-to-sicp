#lang sicp

(define (gcd a b)
  (if (= b 0) a
      (gcd b (remainder a b))))

(define (make-rat n d)
  (let ((g (abs (gcd n d)))
        (newd (abs d))
        (newn (if (> d 0) n (- n))))
       (cons (/ newn g) (/ newd g))))
(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (display (numer x))
  (display "/")
  (display (denom x))
  (newline))

(print-rat (make-rat -1 -4))