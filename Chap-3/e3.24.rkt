#lang sicp


(define (make-table same-key?)
    (let ((local-table (list '*table*))
            (equal? same-key?))
        (define (assoc key list)
            (cond
                ((null? list) false)
                ((equal? key (caar list))
                    (car list))
                (else (assoc key (cdr list)))))
        (define (lookup key-1 key-2)
            (let ((subtable (assoc key-1 (cdr local-table))))
                (if subtable
                    (let ((record (assoc key-2 (cdr subtable))))
                        (if record
                            (cdr record)
                            false))
                    false)))
        (define (insert! key-1 key-2 value)
            (let ((subtable (assoc key-1 (cdr local-table))))
                (if subtable
                    (let ((record (assoc key-2 (cdr subtable))))
                        (if record
                            (set-cdr! record value)
                            (set-cdr! subtable
                                (cons (cons key-2 value)
                                    (cdr subtable)))))
                    (set-cdr! local-table
                        (cons (list key-1
                                (cons key-2 value))
                            (cdr local-table)))))
            'ok)
        (define (dispatch m)
            (cond
                ((eq? m 'lookup-proc) lookup)
                ((eq? m 'insert-proc!) insert!)
                (else (error "Unknown operation: TABLE" m))))
        dispatch))

(define T (make-table (lambda (k1 k2)
                        (if (and (number? k1) (number? k2))
                            (= (remainder k1 3) (remainder k2 3))
                            (equal? k1 k2)))))

(define get (T 'lookup-proc))
(define put (T 'insert-proc!))

(put 1 2 'cat)
(put 2 3 'mouse)
(put 2 9 'dog)

(get 2 3)
(get 4 11)