#lang sicp

(define (make-vect x y)
  (cons x y))
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cdr v))

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))
(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
             (- (ycor-vect v1) (ycor-vect v2))))
(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))

(define (make-segment start end)
  (cons start (sub-vect end start)))
(define (start-segment segment)
  (car segment))
(define (end-segment segment)
  (add-vect (car segment)
            (cdr segment)))

(define (segments->painter segment-list)
  (lambda (frame)
          (for-each
            (lambda (segment)
              (draw-line
                ((frame-coord-map frame)
                 (start-segment segment))
                ((frame-coord-map frame)
                 (end-segment segment))))
            segment-list)))

;; a
(define outline-frame-painter
  (segments->painter (list (make-segment (make-vect 0 0) (make-vect 1 0))
                           (make-segment (make-vect 1 0) (make-vect 1 1))
                           (make-segment (make-vect 1 1) (make-vect 0 1))
                           (make-segment (make-vect 0 1) (make-vect 0 0)))))

;; b
(define x-painter
  (segments->painter (list (make-segment (make-vect 0 0) (make-vect 1 1))
                            (make-segment (make-vect 1 0) (make-vect 0 1)))))

;; c
(define diamond-painter
  (segments->painter (list (make-segment (make-vect 0.5 0) (make-vect 1 0.5))
                            (make-segment (make-vect 1 0.5) (make-vect 0.5 1))
                            (make-segment (make-vect 0.5 1) (make-vect 0 0.5))
                            (make-segment (make-vect 0 0.5) (make-vect 0.5 0)))))

;; wave
(define wave-painter
  (segments->painter (list (make-segment (make-vect 0.4 0) (make-vect 0.5 0.35))
                      (make-segment (make-vect 0.5 0.35) (make-vect 0.6 0))
                      (make-segment (make-vect 0.7 0) (make-vect 0.6 0.55))
                      (make-segment (make-vect 0.6 0.55) (make-vect 1 0.3))
                      (make-segment (make-vect 1 0.35) (make-vect 0.7 0.7))
                      (make-segment (make-vect 0.7 0.7) (make-vect 0.55 0.6))
                      (make-segment (make-vect 0.55 0.6) (make-vect 0.65 0.8))
                      (make-segment (make-vect 0.65 0.8) (make-vect 0.55 1))
                      (make-segment (make-vect 0.45 1) (make-vect 0.35 0.8))
                      (make-segment (make-vect 0.35 0.8) (make-vect 0.45 0.6))
                      (make-segment (make-vect 0.45 0.6) (make-vect 0.3 0.7))
                      (make-segment (make-vect 0.3 0.7) (make-vect 0.1 0.55))
                      (make-segment (make-vect 0.1 0.55) (make-vect 0 0.7))
                      (make-segment (make-vect 0 0.65) (make-vect 0.15 0.4))
                      (make-segment (make-vect 0.15 0.4) (make-vect 0.4 0.55))
                      (make-segment (make-vect 0.4 0.55) (make-vect 0.3 0)))))