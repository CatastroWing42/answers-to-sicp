#lang sicp

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree) result-list)))))
  (copy-to-list tree '()))

(define (make-tree entry left right)
  (list entry left right))
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))

(define tree1
  (make-tree 1 nil (make-tree 2 nil (make-tree 3 nil (make-tree 4 nil (make-tree 5 nil nil))))))
(define tree2
  (make-tree 5
    (make-tree 3 (make-tree 1 nil nil) nil)
    (make-tree 9 (make-tree 7 nil nil)
                 (make-tree 11 nil nil))))

(tree->list-1 tree1)
(tree->list-2 tree1)

(tree->list-1 tree2)
(tree->list-2 tree2)
;; a-> Yes, it produces the same result list

;; Same order Theta(n)
;; but second grows more slowly, it reduces one `append` step on each node
