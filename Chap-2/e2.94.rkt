#lang sicp

(define (div-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (div-terms (term-list p1) (term-list p2)))
      (error "Polys not in same var -- DIV-POLY" p1 p2)))

(define (div-terms L1 L2)
  (if (empty-termlist? L1)
      (list (the-empty-termlist) (the-empty-termlist))
      (let ((t1 (first-term L1))
            (t2 (first-term L2)))
        (if (> (order t2) (order t1))
            (list (the-empty-termlist) L1)
            (let ((new-c (div (coeff t1) (coeff t2)))
                  (new-o (- (order t1) (order t2))))
              (let ((rest-of-result
                      (sub-terms L1 (mul-terms (list (make-term new-o new-c)) L2))))
                (let ((result-of-rest (div-terms rest-of-result L2)))
                  (list (adjoin-term
                          (make-term new-o new-c)
                          (car (div-terms rest-of-result L2)))
                        (cadr (div-terms rest-of-result L2))))))))))

(define (remainder-terms L1 L2)
  (cadr (div-terms L1 L2)))

(define (gcd-terms a b)
  (if (empty-termlist? b)
      a
      (gcd-terms b (remainder-terms a b))))

(define (install-polynomial-package)
  (define (make-poly variable term-list) (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1) (term-list p2)))
        (error "Polys not in same var -- ADD-POLY" p1 p2)))
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1) (term-list p2)))
        (error "Polys not in same var -- MUL-POLY" p1 p2)))
  (define (gcd-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (gcd-terms (term-list p1) (term-list p2)))
        (error "Polys not in same var -- GCD-POLY" p1 p2)))
  ;;interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
    (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
    (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'gcd '(polynomial polynomial)
    (lambda (p1 p2) (tag (gcd-poly p1 p2))))
  (put 'make 'polynomial
    (lambda (var terms) (tag (make-poly var terms))))
  'done)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (drop (apply proc (map contents args)))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (higher-type? type1 type2)
                    (apply-generic op a1 (raise-one-level a2))
                    (apply-generic op (raise-one-level a1) a2)))
              (error "No method for these types" (list op type-tags)))))))

(define (greatest-common-divisor a b)
  (apply-generic 'gcd a b))

(define p1 (make-polynomial
             'x '((4 1) (3 -1) (2 -2) (1 2))))
(define p2 (make-polynomial
             'x '((3 1) (1 -1))))
(greatest-common-divisor p1 p2)
