#lang sicp

; a
(meeting ?d (Friday ?t))

; b
(rule (meeting-time ?person ?day-and-time)
    (or (meeting whole-company ?day-and-time)
        (and (job ?person (?division . ?rest))
            (meeting ?division ?day-and-time))))

; c
(meeting-time (Hacker Alyssa P) (Wednesday ?time))