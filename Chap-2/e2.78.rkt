#lang sicp

(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) 'scheme-number)
        (error "Incorrect data -- TYPE-TAG" datum)))
(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)
        (error "Incorrect data -- CONTENTS" datum)))
(define (attach-tag type-tag contents)
  (if (number? contents) contents (cons type-tag contents)))