#lang sicp

(define (sum term a next b)
  (if (> a b) 0
    (+ (term a) (sum term (next a) next b))))

(define (simpson-integral f a b n)
  (define (do-simpson-integral f a b h)
    (define (simpson-term k)
      (define (y k)
        (f (+ a (* k h))))
      (* 
        (cond ((= k 0) 1)
              ((= k n) 1)
              ((even? k) 2)
              (else 4))
        (y k)))
    (define (simpson-next k) (+ k 1))
    (* (/ h 3) (sum simpson-term 0 simpson-next n)))
  (do-simpson-integral f a b (/ (- b a) n)))

(define (cube x) (* x x x))

(simpson-integral cube 0 1 1000)