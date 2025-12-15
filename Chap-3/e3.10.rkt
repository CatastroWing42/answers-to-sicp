#lang sicp

(define (make-withdraw initial-amount)
    (let ((balance initial-amount))
        (lambda (amount)
            (if (>= balance amount)
                (begin (set! balance (- balance amount))
                    balance)
                "Insufficient funds"))))

(let ((name exp)) body)
=> ((lambda (name) body) exp)

:=>
(define (make-withdraw initial-amount)
    ((lambda (balance)
        (lambda (amount)
            (if (>= balance amount)
                (begin (set! balance (- balance amount))
                    balance)
                "Insufficient funds")))
        initial-amount))
