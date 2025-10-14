#lang sicp

(define (pascal-triangle-element row column)
  (cond ((= column 1) 1)
        ((= row column) 1)
        (else (+ (pascal-triangle-element (- row 1) (- column 1))
                 (pascal-triangle-element (- row 1) column)))))

(pascal-triangle-element 5 3)