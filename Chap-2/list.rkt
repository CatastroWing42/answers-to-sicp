#lang sicp

(define test (list 1 2 3))

(cdddr test)
(length test)

(define f1 (lambda () (display "no arguments")
                      (newline)))
(f1)
(define f2 (lambda w (car w)))
(f2 1 2 3)
(even? -2)
(append (list 1 2) (list))
(length (list))
(define x (list 1))
(pair? (car x))
(pair? (cdr x))
(pair? nil)

"---------"
nil
(list)
(list nil)
(inc 3)
