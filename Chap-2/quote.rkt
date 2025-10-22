#lang sicp

;; (define a 2)
;; (define b 3)
;; (define c 4)

;; (list a '(b c))

;; (display (car '(a b c)))

(define a 1)
(define b 1)
(append (list 1 2) '())
(append (list 1 2) nil)

(eq? a b)
(eq? a 1)
(eq? 'a 'b)
(eq? a 'b)
(eq? nil 'nil)

(symbol? 'a)
(symbol? (cadr '(+ a 1)))
(symbol? (car '(+ a 1)))
(number? (caddr '(+ a 1)))

;; (=number? a 1)
(expt 2 3)
(define (f a . b)
  (+ a (length b)))
(f 1 2 3 4)

(quotient 10 3)
(quotient 12 3)