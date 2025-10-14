#lang sicp

(define (cube x) (* x x x))
(define (p x) (display "1 ") (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

(sine 12.15) ;; 12.15 / 0.1 = 121.5 => need 3^5

;; space: theta(log3(a))
;; time: theta(log3(a))