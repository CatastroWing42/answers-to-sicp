#lang sicp

(rule (lives-near ?person-1 ?person-2)
    (and (address ?person-1 (?town . ?rest-1))
        (address ?person-2 (?town . ?rest-2))
        (lisp-value lower-than ?person-1 ?person-2)))