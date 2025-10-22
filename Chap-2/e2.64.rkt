#lang sicp

;; balanced tree will have 1 root entry and 2 subtrees, the node number of each subtree
;; should be equal or different by 1
;; list->tree for the list (1 3 5 7 9 11) will be
;;          5
;;        /   \
;;       1     9
;;        \   / \
;;         3 7  11

(define (make-tree entry left right)
  (list entry left right))
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))

(define (list->tree elements)
  (partial-tree elements (length elements)))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size))
              (right-size (- n (+ left-size 1))))
          (let ((this-entry (cadr left-result))
                (left-tree (car left-result))
                (right-result (partial-tree (cddr left-result) right-size)))
            (let ((right-tree (car right-result))
                  (remaining-elets (cdr right-result)))
                 (cons (make-tree this-entry left-tree right-tree)
                       remaining-elets)))))))

(list->tree '(1 3 5 7 9 11))

;; b->Theta(n)