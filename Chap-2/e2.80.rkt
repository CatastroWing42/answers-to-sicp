#lang sicp

(define (=zero? x)
  (apply-generic '=zero? x))

(define (install-=zero?-package)
  (put '=zero? '(scheme-number) (lambda (x) (= x 0)))
  (put '=zero? '(rational) (lambda (x) (= (numer x) 0)))
  (put '=zero? '(complex)
    (lambda (z) (apply-generic '=zero? z)))
  (put '=zero? '(polar) (= (magnitude z) 0))
  (put '=zero? '(rectangular)
    (lambda (z) (and (= (real-part z) 0) (= (imag-part z) 0))))
  'done)