#lang sicp

(define (square x) (* x x))
(define (square-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

(square-tree (list 1 (list 2 (list 3 4) 5)))

(define (square-tree-2 tree)
  (map (lambda (sub-tree)
        (if (pair? sub-tree) (square-tree-2 sub-tree)
          (square sub-tree))) tree))

(square-tree-2 (list 1 (list 2 (list 3 4) 5)))
