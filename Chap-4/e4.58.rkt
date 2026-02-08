#lang sicp

(rule (big-shot ?person ?division)
    (and (job ?person (?division . ?rest))
        (supervisor ?person ?sup)
        (not (job ?sup (?division . ?sup-rest)))))