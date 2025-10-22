#lang sicp

; a: it transforms the deriv procedure to a generic procedure using data-directed programming
; dont need to do that, besides number? and variable? doesnt share the same pattern with operator?

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp) var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (intall-sum-package)
  (define (deriv-sum operands var)
    (make-sum (deriv (car operands) var)
              (deriv (cadr operands) var)))
  (put 'deriv '+ deriv-sum))

(define (install-product-package)
  (define (deriv-product operands var)
    (make-sum (make-product (car operands)
                (deriv (cadr operands) var))
              (make-product (cadr operands)
                (deriv (car operands) var))))
  (put 'deriv '* deriv-product))


(define (install-exponent-package)
  (define (deriv-exponent operands var)
    (make-product (make-product (cadr operands)
                    (make-product (make-exponentiation (car operands)
                                    (- (cadr operands) 1))))))
  (put 'deriv '** deriv-exponent))

; d: need to change install-* procedures, swap put keys