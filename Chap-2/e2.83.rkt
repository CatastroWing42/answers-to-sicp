#lang sicp

(define (make-real rat irrat multiplier)
  (list 'real rat irrat multiplier))

(put 'raise-one-level 'integer (lambda (x) (make-rational (number x) 1)))
(put 'raise-one-level 'rational (lambda (x) (make-real (/ (numer x) (denom x)) 0 0)))
(put 'raise-one-level 'real (lambda (x) (make-complex-from-real-imag
                                          (+ (rat x) (* (irrat x) (multiplier x))) 0)))

(define (raise-one-level datum)
  (let ((type (car datum)))
    (if (eq? type 'complex) datum
      (let ((proc (get 'raise-one-level type)))
        (if proc
            (proc datum)
            (error "No method for these types -- RAISE-ONE-LEVEL" type))))))