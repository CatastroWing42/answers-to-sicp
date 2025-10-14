#lang sicp

(define (square x) (* x x))

(define (make-point x y)
  (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (make-segment start end)
  (cons start end))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))
(define (segment-length s)
  (sqrt (+ (square (- (x-point (start-segment s)) (x-point (end-segment s))))
           (square (- (y-point (start-segment s)) (y-point (end-segment s)))))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

;;---one way---
#|
(define (make-rect s1 s2 s3)
  (cons s1 (cons s2 s3)))
(define (rect-side1 rect)
  (car rect))
(define (rect-side2 rect)
  (car (cdr rect)))
(define (rect-side3 rect)
  (cdr (cdr rect)))
|#
;; ---another way---
(define (make-rect p1 p2 p3)
  (cons p1 (cons p2 p3)))
(define (rect-side1 rect)
  (make-segment (car rect) (car (cdr rect))))
(define (rect-side2 rect)
  (make-segment (car (cdr rect)) (cdr (cdr rect))))
(define (rect-side3 rect)
  (make-segment (cdr (cdr rect)) (car rect)))
;;---

(define (rect-side1-length rect)
  (segment-length (rect-side1 rect)))
(define (rect-side2-length rect)
  (segment-length (rect-side2 rect)))
(define (rect-side3-length rect)
  (segment-length (rect-side3 rect)))

(define (perimeter rect)
  (+ (rect-side1-length rect) (rect-side2-length rect) (rect-side3-length rect)))
(define (area rect)
  (let ((s (/ (perimeter rect) 2))
        (a (rect-side1-length rect))
        (b (rect-side2-length rect))
        (c (rect-side3-length rect)))
       (sqrt (* s (- s a) (- s b) (- s c)))))

#|
(define test-rect (make-rect (make-segment (make-point 0 0) (make-point 1 0))
                             (make-segment (make-point 0 0) (make-point 0 1))
                             (make-segment (make-point 1 0) (make-point 0 1))))
|#
(define test-rect (make-rect (make-point 0 0)
                             (make-point 1 0)
                             (make-point 0 1)))

(perimeter test-rect)
(area test-rect)