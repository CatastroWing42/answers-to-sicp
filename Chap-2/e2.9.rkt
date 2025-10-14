#lang sicp

(define (make-interval a b)
  (if (> a b) (cons b a) (cons a b)))
(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))
                
(define (width x) (/ (- (upper-bound x) (lower-bound x)) 2))

;; x + y = (Lx + Ly, Ux + Uy) 
;; =>width(x + y) = (Ux - Lx) / 2 + (Uy - Ly) / 2 = width(x) + width(y)

;; x - y = (Lx - Uy, Ux - Ly)
;; =>width(x - y) = (Ux - Lx) / 2 + (Uy - Ly) / 2 = width(x) + width(y)
(define interval1 (make-interval 1 2))
(define interval2 (make-interval 3 6))
(width interval1)
(width interval2)
(width (add-interval interval1 interval2))
(width (sub-interval interval1 interval2))

;; Give that x = (2, 5) y = (3, 7)
;; width(x) = 1.5 width(y) = 2
;; x * y = (2 * 3, 5 * 7) = (6, 35)
;; width(x * y) = 14.5
;; width(x * y) = (Ux * Uy - Lx * Ly) / 2
;; 
;; x / y = (Lx / Uy, Ux / Ly) = (2 / 7, 5 / 3)
;; width(x / y) = (Ux / Ly - Lx / Uy) / 2 = 29/21