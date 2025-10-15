#lang sicp

(define (accumulate op initial sequence)
  (if (null? sequence) initial
    (op (car sequence) (accumulate op initial (cdr sequence)))))

(define (flatmap proc s)
  (accumulate append nil (map proc s)))

(define (enumerate-number low high)
  (if (> low high) nil
    (cons low (enumerate-number (+ low 1) high))))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence)) (cons (car sequence) (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (make-queen row col)
  (cons row col))
(define (queen-row q) (car q))
(define (queen-col q) (cdr q))

(define (adjoin-position new-row k rest-of-queens)
  (let ((this-queen (make-queen new-row k)))
       (cons this-queen rest-of-queens)))
(define (safe-queens? q1 q2)
  (let ((row1 (queen-row q1))
        (col1 (queen-col q1))
        (row2 (queen-row q2))
        (col2 (queen-col q2)))
       (and (not (= row1 row2))
            (not (= col1 col2))
            (not (= (abs (- row1 row2)) (abs (- col1 col2)))))))
(define (safe? k positions)
  (let ((k-th-queen (car positions)))
       (accumulate (lambda (q r)
                           (and (safe-queens? k-th-queen q) r))
                   true
                   (cdr positions))))
(define empty-board nil)
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                  (adjoin-position
                    new-row k rest-of-queens))
                 (enumerate-number 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(length (queens 6))