#lang sicp

;; a
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
                      (make-segment (make-vect 0.4 0.55) (make-vect 0.3 0))
                      (make-segment (make-vect 0.45 0.8) (make-vect 0.55 0.8)))));; add a mouth segment

;; b
(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
           (let ((top-left up);; use one
                 (bottom-right right);; use one
                 (corner (corner-split painter (- n 1))))
             (beside (below painter top-left)
                     (below bottom-right corner))))))

;; c
(define (square-limit painter n)
  (let ((combine4 (suqare-of-four identity flip-horiz rotate90 rotate180)))
    (combine4 (corner-split painter n))))