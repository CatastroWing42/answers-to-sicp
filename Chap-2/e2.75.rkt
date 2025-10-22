#lang sicp

(define (make-from-mag-ang magnitude angle)
  (define (dispatch op)
    (cond ((eq? op 'real) (* magnitude (cos angle)))
          ((eq? op 'imag) (* magnitude (sin angle)))
          ((eq? op 'magnitude) magnitude)
          ((eq? op 'angle) angle)
          (else (error "Unknown op -- MAKE-FROM-MAG-ANG" op))))
  dispatch)