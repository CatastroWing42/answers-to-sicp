#lang sicp

(define (sub-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (sub-terms (term-list p1) (term-list p2)))
      (error "Polys not in same var -- ADD-POLY" p1 p2)))

(define (negation-terms L)
  (if (empty-termlist? L)
      (the-empty-termlist)
      (let ((t (first-term L)))
        (adjoin-term
          (make-term (order t)
                     (negation (coeff t)))
          (negation-terms (rest-terms L))))))
(define (sub-terms L1 L2)
  (add-terms L1 (negation-terms L2)))

(define (negation x)
  (apply-generic 'negation x))

(define (install-negation-package)
  (put 'negation '(scheme-number) (lambda (x) (- x)))
  (put 'negation '(polynomial)
    (lambda (p) (make-poly (variable p) (negation-terms (term-list p)))))
  'done)