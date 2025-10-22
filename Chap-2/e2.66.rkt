#lang sicp

(define (make-tree entry left right)
  (list entry left right))
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        (else (let ((entry-key (key (entry set-of-records))))
                   (cond ((equal? given-key entry-key) (entry set-of-records))
                         ((smaller? given-key entry-key) (lookup given-key (left-branch set-of-records)))
                         (else (lookup given-key (right-branch set-of-records))))))))