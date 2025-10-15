#lang sicp

(define (make-mobile left right)
  (list left right))
(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
 (car (cdr mobile)))

(define (make-branch length structure)
  (list length structure))
(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (car (cdr branch)))

(define (branch-weight branch)
  (let ((structure (branch-structure branch)))
    (if (pair? structure) (total-weight structure) structure)))
(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

(define (torque branch)
  (* (branch-length branch) (branch-weight branch)))
(define (balanced? mobile)
  (if (not (pair? mobile)) true
  (let ((left (left-branch mobile))
        (right (right-branch mobile)))
  (and (= (torque left)
          (torque right))
       (balanced? (branch-structure left))
       (balanced? (branch-structure right))))))

;; If change representation of mobile and branch.
;; We need only to change left-branch right-branch branch-structure and branch-length