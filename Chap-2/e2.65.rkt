#lang sicp

(define (tree->list tree)
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

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

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

(define (union-ordered-list list1 list2)
  (cond ((null? list1) list2)
        ((null? list2) list1)
        (else (let ((x1 (car list1)) (x2 (car list2)))
                   (cond ((< x1 x2)
                           (cons x1 (union-ordered-list (cdr list1) list2)))
                         ((= x1 x2)
                           (cons x1 (union-ordered-list (cdr list1) (cdr list2))))
                         (else (cons x2 (union-ordered-list list1 (cdr list2)))))))))
(define (intersection-ordered-list list1 list2)
  (cond ((or (null? list1) (null? list2)) nil)
        (else (let ((x1 (car list1)) (x2 (car list2)))
                   (cond ((< x1 x2) (intersection-ordered-list (cdr list1) list2))
                         ((= x1 x2) (cons x1 (intersection-ordered-list (cdr list1) (cdr list2))))
                         (else (intersection-ordered-list list1 (cdr list2))))))))

(define (union-set set1 set2)
  (list->tree
   (union-ordered-list (tree->list set1) (tree->list set2))))

(define (intersection-set set1 set2)
  (list->tree
   (intersection-ordered-list (tree->list set1) (tree->list set2))))

(define set1 (list->tree '(1 2 3 4 5 6 7 8 9 10)))
(define set2 (list->tree '(1 3 5 7 9 11 13 15 17 19)))

; (union-ordered-list '(1 2 3 4 5 6 7 8 9 10) '(1 3 5 7 9 11 13 15 17 19))
; (intersection-ordered-list '(1 2 3 4 5 6 7 8 9 10) '(1 3 5 7 9 11 13 15 17 19))

; (tree->list (list->tree '(1 2 3 4 5 6 7 8 9 10)))

(union-set (list->tree '(1 2 3 4 5 6 7 8 9 10))
           (list->tree '(1 3 5 7 9 11 13 15 17 19)))
(intersection-set (list->tree '(1 2 3 4 5 6 7 8 9 10))
           (list->tree '(1 3 5 7 9 11 13 15 17 19)))