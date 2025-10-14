#lang sicp

;; No! its not possible.
;; The essential part of this problem is about the formula `A/A = one`
;; consider a situation where B = A = [1, 2], according to the evaluation rule of interpreter,
;; we can't tell if [1, 2]/[1, 2] should be one(algebraic division)
;; or [1/2, 2](interval arithmetic)

;; take the example of (R1 * R2) / (R1 + R2) = 1 / (1 / R1 + 1 / R2)
;; this equation is algebraically right because R1 and R2 are perfect symbols,
;; their information is intactly represented in the symbol itself, safe to manipulate.
;; but computer programs are not the same. Information will lost during process of R1 * R2
;; and (R1 + R2). Two new `intervals` are created, and the original R1 and R2 has been flushed away.

;; One attempt is to save information about the original R1 and R2, but this is not possible.