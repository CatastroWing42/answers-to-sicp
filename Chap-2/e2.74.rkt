#lang sicp

(define (error-proc) (error "Invalid procedure"))

; personnel-file should be structured like (cons division-name employee-records)
; employee-records should be implemented as a set, keyed by employee-name
(define (division-name personnel-file)
  (car personnel-file))

; global table should contain `get-record` procedures indexed by (op-name division-name)
;              | division-1   | division-2
;   ---------------------------------------
;   get-record | get-record-1 | get-record-2
(define (get-record personnel-file employee-name)
  ((get 'get-record (division-name personnel-file)) employee-name))


; employee-record should be structured like (cons employee-name employee-data)
; employee-data should be implemented as a set, keyed by identifiers
(define (employee-name employee-record)
  (car employee-record))
(define (get-salary employee-record)
  ((get 'get-salary (employee-name employee-record)) 'salary))
; this makes global table looks like
;              | division-1   | division-2    | employee-1   | employee-2
;   ---------------------------------------------------------------------
;   get-record | get-record-1 | get-record-2  | error-proc   | error-proc
;   get-salary | error-proc   | error-proc    | get-salary-1 | get-salary-2

(define (accumulate op initial sequence)
  (if (null? sequence) initial
    (op (car sequence) (accumulate op initial (cdr sequence)))))

; answer to prob.c: 
(define (find-employee-record employee-name personnel-files)
  (accumulate (lambda (x y)
                (let ((employee-record (get-record x employee-name)))
                  (if (pair? employee-record)
                    (cons employee-record y))))
              nil
              personnel-files))

; answer to prob.d:
; no change to generic procedures, only need a (install-<new company>-package)
; call that procedure to install new company's package and thats all