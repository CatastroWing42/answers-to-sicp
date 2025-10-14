#lang sicp

(define (make-interval a b)
  (if (> a b) (cons b a) (cons a b)))
(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

(define (make-center-percent center percent)
  (make-interval (- center (* center percent))
                 (+ center (* center percent))))
(define (center x)
  (/ (+ (lower-bound x) (upper-bound x)) 2))
(define (percent x)
  (/ (/ (- (upper-bound x) (lower-bound x)) 2) (center x)))

(percent (make-center-percent 100 0.1))