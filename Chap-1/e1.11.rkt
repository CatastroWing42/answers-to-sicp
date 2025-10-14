#lang sicp

#|
(define (f n)
  (if (< n 3) n
    (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))))))

(f 30)

|#
(define (f-iter n)
  (define (iter a b c counter)
    (if (< counter 3) c
      (iter b c (+ (* 3 a) (* 2 b) c) (- counter 1))))
  (if (< n 3) n (iter 0 1 2 n)))

"--------------------"
(f-iter 100)