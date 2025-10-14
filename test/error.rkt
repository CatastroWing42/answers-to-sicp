#lang sicp

(define (f n)
  (cond ((= n 0) 33)
        ((= n 1) (error "1 out"))
        (else (f (- n 1)))))

(f 10)