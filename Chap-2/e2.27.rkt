#lang sicp

(define (reverse l)
  (define (iter l result)
    (if (null? l) result
      (iter (cdr l) (cons (car l) result))))
  (iter l nil))

(define (deep-reverse items)
  (define (iter items result)
    (if (null? items) result
      (iter (cdr items) (cons (deep-reverse (car items)) result))))
  (if (pair? items) (iter items nil) items))

(define x (list (list 1 2) (list 3 4)))
(reverse x)
(deep-reverse x)