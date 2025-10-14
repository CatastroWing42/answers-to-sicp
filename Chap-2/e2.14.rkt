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

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
        (make-interval (min p1 p2 p3 p4)
                       (max p1 p2 p3 p4))))
(define (span-zero? x)
  (and (<= (lower-bound x) 0) (>= (upper-bound x) 0)))
(define (div-interval x y)
  (if (span-zero? y) (error "div-interval: span zero")
    (mul-interval x
      (make-interval (/ 1.0 (upper-bound y))
                     (/ 1.0 (lower-bound y))))))

(define (make-center-percent center percent)
  (make-interval (- center (* center percent))
                 (+ center (* center percent))))
(define (center x)
  (/ (+ (lower-bound x) (upper-bound x)) 2))
(define (percent x)
  (/ (/ (- (upper-bound x) (lower-bound x)) 2) (center x)))


(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2) (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
       (div-interval one (add-interval (div-interval one r1) (div-interval one r2)))))

(define A (make-interval 1 2))
(define B (make-interval 3 4))
(par1 A B)
(par2 A B)
"--------"
(div-interval A B)
(div-interval A A)
"--------"
(define C (make-center-percent 100 0.01))
(div-interval C C)
(define D (make-center-percent 137 0.001))
(define result1 (par1 C D))
(define result2 (par2 C D))
"--------"
(center result1)
(percent result1)
(center result2)
(percent result2)