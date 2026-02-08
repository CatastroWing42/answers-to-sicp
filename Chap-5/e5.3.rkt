#lang sicp

; good-enough? and improve as primitive
(controller
    (assign guess (const 1.0))
    test-good
        (test (op good-enough?) (reg guess))
        (branch (label sqrt-done))
        (assign guess (op improve) (reg guess))
        (goto test-good)
    sqrt-done)

; more elaborate machine controller
(controller
    (assign guess (const 1.0))
    test-good
        (assign t (op square) (reg guess))
        (assign t (op -) (reg t) (reg x))
        (assign t (op abs) (reg t))
        (test (op <) (reg t) (const 0.001))
        (branch (label sqrt-done))
        (assign t (op /) (reg x) (reg guess))
        (assign guess (op average)
            (reg guess) (reg t))
        (goto test-good)
    sqrt-done)