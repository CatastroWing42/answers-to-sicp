#lang sicp

(define (make-interval a b)
  (if (> a b) (cons b a) (cons a b)))
(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(define interval1 (make-interval 1 2))
(define interval2 (make-interval 3 4))
(sub-interval interval1 interval2)