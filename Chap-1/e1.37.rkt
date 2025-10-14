#lang sicp

(define (cont-frac n d k)
  (define (unfold i)
    (if (= i k) (/ (n i) (d i))
       (/ (n i) (+ (d i) (unfold (+ i 1))))))
  (unfold 1))

(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 11) ;; this will generate 0.6180

(define (cont-frac-iter n d k)
  (define (iter i result)
    (if (= i 0) result
      (iter (- i 1) (/ (n i) (+ (d i) result)))))
  (iter k 0))

(cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 11) ;; this will generate 0.6180