#lang sicp

(define (cont-frac n d k)
  (define (iter i result)
    (if (= i 0) result
      (iter (- i 1) (/ (n i) (+ (d i) result)))))
  (iter k 0))

(define (euler-expansion k)
  (define (essential i)
    (= (remainder i 3) 2))
  (+ 2 (cont-frac (lambda (i) 1.0)
                  (lambda (i)
                    (if (essential i) (/ (* 2 (+ i 1)) 3) 1)) k)))

(euler-expansion 11)