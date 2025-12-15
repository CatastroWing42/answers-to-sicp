#lang sicp

(define (same-keys-general? keys-1 keys-2)
    (cond
        ((and (null? keys-1) (null? keys-2)) true)
        ((or (null? keys-1) (null? keys-2)) false)
        ((equal? (car keys-1) (car keys-2))
            (same-keys-general? (cdr keys-1) (cdr keys-2)))
        (else false)))

(define (make-general-table same-keys?)
    (let ((local-table (list '*general-table*)))
        (define (assoc keys list)
            (cond
                ((null? list) false)
                ((same-keys? keys (caar list))
                    (car list))
                (else (assoc keys (cdr list)))))
        (define (lookup keys)
            (let ((record (assoc keys (cdr local-table))))
                (if record
                    (cdr record)
                    false)))
        (define (insert! keys value)
            (let ((record (assoc keys (cdr local-table))))
                (if record
                    (set-cdr! record value)
                    (set-cdr! local-table
                        (cons (cons keys value)
                            (cdr local-table))))))
        (define (dispatch m)
            (cond
                ((eq? m 'lookup-proc) lookup)
                ((eq? m 'insert-proc!) insert!)
                (else (error "Unknown operation: TABLE" m))))
        dispatch))

(define operation-table (make-general-table same-keys-general?))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

; test
(put (list 'a 'b 3) 'cat)
(put (list 'a 'b) 'dog)
(put (list 'a 'c) 'elephant)
(put (list 'a 'b 3) 'catee)
(get (list 'a))
(get (list 'a 'b 3))