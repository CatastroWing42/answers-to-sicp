#lang sicp

(define balance 100)
Peter: (set! balance (- balance 10))
Paul (set! balance (- balance 20))
Mary (set !balance (- balance (/ balance 2)))

;a
; 35 25 30 20

;b
;90 80 50
;70 45 40 30
;35 25 20