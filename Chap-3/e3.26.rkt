#lang sicp

(define (make-tree)
    (define (make-tree-node node)
        (list node '() '()))
    (define (tree-node tree)
        (car tree))
    (define (left-branch tree)
        (cadr tree))
    (define (right-branch tree)
        (caddr tree))
    (define (set-left-branch! tree left)
        (set-car! (cdr tree) left))
    (define (set-right-branch! tree right)
        (set-car! (cddr tree) right))
    (let ((local-tree '()))
        (define (lookup-tree node tree)
            (if (null? tree)
                false
                (let ((cur-node (tree-node tree)))
                    (cond
                        (((node 'equal?) cur-node) cur-node)
                        (((node 'smaller?) cur-node)
                            (lookup-tree node (left-branch tree)))
                        (else
                            (lookup-tree node (right-branch tree)))))))
        (define (insert-tree! node tree)
            (if (null? tree)
                (make-tree-node node)
                (cond
                    (((node 'equal?) (tree-node tree))
                        (error "Node exists: TREE"))
                    (((node 'smaller?) (tree-node tree))
                        (begin
                            (set-left-branch! tree
                                (insert-tree! node (left-branch tree)))
                            tree))
                    (else 
                        (begin
                            (set-right-branch! tree
                                (insert-tree! node (right-branch tree)))
                            tree)))))
        (define (lookup node)
            (lookup-tree node local-tree))
        (define (insert! node)
            (set! local-tree (insert-tree! node local-tree)))
        (define (dispatch m)
            (cond
                ((eq? m 'lookup-proc) lookup)
                ((eq? m 'insert-proc!) insert!)
                (else (error "Unknown operation: TREE" m))))
        dispatch))

(define (same-keys? keys-1 keys-2)
    (cond
        ((and (null? keys-1) (null? keys-2)) true)
        ((or (null? keys-1) (null? keys-2)) false)
        ((equal? (car keys-1) (car keys-2))
            (same-keys? (cdr keys-1) (cdr keys-2)))
        (else false)))
(define (smaller-keys? keys-1 keys-2)
    (cond
        ((null? keys-1)
            (cond
                ((null? keys-2) false)
                (else true)))
        (else
            (cond
                ((null? keys-2) false)
                (else
                    (let ((k1 (car keys-1))
                            (k2 (car keys-2)))
                        (cond
                            ((equal? k1 k2) (smaller-keys? (cdr keys-1) (cdr keys-2)))
                            ((and (number? k1) (number? k2)) (< k1 k2))
                            (else false))))))))

(define (make-tree-table)
    (define (make-node keys value)
        (define (node-keys) keys)
        (define (node-value) value)
        (define (node-equal? other-node)
            (same-keys? keys ((other-node 'get-keys))))
        (define (set-node-value! new-value)
            (set! value new-value))
        (define (node-smaller? other-node)
            (smaller-keys? keys ((other-node 'get-keys))))
        (define (dispatch m)
            (cond
                ((eq? m 'get-keys) node-keys)
                ((eq? m 'get-value) node-value)
                ((eq? m 'set-value!) set-node-value!)
                ((eq? m 'equal?) node-equal?)
                ((eq? m 'smaller?) node-smaller?)))
        dispatch)
    (let ((local-table (list '*tree-table* (make-tree))))
        (define (lookup keys)
            (let ((trival-node (make-node keys '()))
                    (local-table-tree (cadr local-table)))
                (let ((record ((local-table-tree 'lookup-proc) trival-node)))
                    (if record
                        ((record 'get-value))
                        false))))
        (define (insert! keys value)
            (let ((insert-node (make-node keys value)))
                (let ((record (((cadr local-table) 'lookup-proc) insert-node)))
                    (if record
                        ((record 'set-value!) value)
                        (((cadr local-table) 'insert-proc!) insert-node)))))
        (define (dispatch m)
            (cond
                ((eq? m 'lookup-proc) lookup)
                ((eq? m 'insert-proc!) insert!)
                (else (error "Unknown operation: TABLE" m))))
        dispatch))

(define T (make-tree-table))
(define get (T 'lookup-proc))
(define put (T 'insert-proc!))

; test
(put (list 'a 'b 3) 'cat)
(put (list 'a 'b) 'dog)
(put (list 'a 'c) 'elephant)
(put (list 'a 'b 3) 'catee)
(get (list 'a))
(get (list 'a 'b 3))