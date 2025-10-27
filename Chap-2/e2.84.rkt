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

(define (higher-type? type1 type2)
  (define (type-number type)
    (cond ((eq? type 'integer) 0)
          ((eq? type 'rational) 1)
          ((eq? type 'real) 2)
          ((eq? type 'complex) 3)))
  (> (type-number type1) (type-number type2)))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (higher-type? type1 type2)
                    (apply-generic op a1 (raise-one-level a2))
                    (apply-generic op (raise-one-level a1) a2)))
              (error "No method for these types" (list op type-tags)))))))