#lang sicp

(define (average x y) (/ (+ x y) 2))
(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (compose f g)
  (lambda (x) (f (g x))))
(define (repeated f n)
  (if (= n 1) f (compose f (repeated f (- n 1)))))

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try-it guess counter)
    (let ((next (f guess)))
          (if (close-enough? guess next)
             next
             (try-it next (+ counter 1)))))
  (try-it first-guess 1))

(define (nth-root n x damp-count)
  (fixed-point ((repeated average-damp damp-count)
                (lambda (y) (/ x (expt y (- n 1))))) 1.0))

(nth-root 9 2 3)