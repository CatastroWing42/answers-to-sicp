#lang sicp

(define (cont-frac n d k)
  (define (iter i result)
    (if (= i 0) result
      (iter (- i 1) (/ (n i) (+ (d i) result)))))
  (iter k 0))

(define (square x) (* x x))
(define (tan-cf x k)
  (cont-frac (lambda (i)
               (if (= i 1) x (- (square x))))
             (lambda (i) (- (* 2 i) 1)) k))

(tan-cf 0.5 10)