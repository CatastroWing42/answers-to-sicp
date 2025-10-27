#lang sicp

(define (magnitude z)
  (apply-generic 'magnitude z))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error "No method for these types -- APPLY-GENERIC" (list op type-tags))))))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Incorrect data -- TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Incorrect data -- CONTENTS" datum)))

(put 'magnitude '(complex) magnitude)

; apply-generic is called twice
; first dispatched (magnitude (complex..)) to (magnitude (rectangular..))
; second dispatched (magnitude (rectangular..)) to (magnitude (real-part..))
; the latter will do the real computation