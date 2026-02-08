#lang sicp

(rule (same ?x ?x))
(rule (replace ?person-1 ?person-2)
    (and (or (and (job ?person-1 ?job)
                (job ?person-2 ?job))
            (and (job ?person-1 ?job-1)
                (job ?person-2 ?job-2)
                (can-do-job ?job-1 ?job-2)))
        (not (same ?person-1 ?person-2))))

; a
(replace ?x (Fect Cy D))

; b
(and (replace ?x ?y)
    (salary ?x ?sx)
    (salary ?y ?sy)
    (lisp-value > ?sy ?sx))