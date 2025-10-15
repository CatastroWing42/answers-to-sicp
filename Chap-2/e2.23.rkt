#lang sicp

(define (for-each f l)
  (define (do f l)
    (f (car l))
    (for-each f (cdr l)))
  (if (null? l) (newline) (do f l)))

(for-each display (list 1 2 3 4))
(for-each (lambda (x)
            (newline)
            (display x))
          (list 57 321 88))