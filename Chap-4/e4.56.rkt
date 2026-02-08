#lang sicp

(and (supervisor ?x (Bitdiddle Ben))
    (address ?x ?y))

(and (salary ?x ?a)
    (salary (Bitdiddle Ben) ?b)
    (lisp-value < ?a ?b))

(and (supervisor ?x ?y)
    (not (job ?y (computer . ?d)))
    (job ?y ?j))