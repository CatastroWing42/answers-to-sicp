#lang sicp

(define (filtered-accumulate filter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b) result
      (iter (next a) (if (filter a)
                         (combiner result (term a))
                         result))))
  (iter a null-value))

(define (square x) (* x x))
(define (inc x) (+ x 1))

(define (fermat-test n)
    (define (expmod base exp m)
        (cond ((= exp 0) 1)
              ((even? exp)
               (remainder (square (expmod base (/ exp 2) m)) m))
              (else
               (remainder (* base (expmod base (- exp 1) m)) m))))
    (define (try-it a)
      (= (expmod a n n) a))
    (try-it (+ 1 (random (- n 1)))))
(define (fast-prime? n times)
    (cond ((= times 0) true)
          ((fermat-test n) (fast-prime? n (- times 1)))
          (else false)))

(define (prime? n) (fast-prime? n 5))

(define (sum-of-squares-of-prime a b)
  (filtered-accumulate prime? + 0 square a inc b))

(sum-of-squares-of-prime 2 10)

"---------------"

(define (product-of-prime-to-n n)
  (define (prime-to-n a)
    (= (gcd a n) 1))
  (filtered-accumulate prime-to-n * 1 identity 1 inc n))

(product-of-prime-to-n 10)
