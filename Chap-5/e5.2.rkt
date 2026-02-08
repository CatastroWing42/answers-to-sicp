#lang sicp

(define (factorial n)
    (define (iter product counter)
        (if (> counter n)
            product
            (iter (* product counter)
                (+ counter 1))))
    (iter 1 1))

(controller
    test-b
        (test (op >) (reg counter) (reg n))
        (branch (label factorial-done))
        (assign product (op *) (reg product) (reg counter))
        (assign counter (op +) (reg counter) (const 1))
        (goto (lable test-b))
    factorial-done)