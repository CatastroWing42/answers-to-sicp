#lang sicp

((lambda (n)
    ((lambda (fact) (fact fact n))
        (lambda (ft k) (if (= k 1) 1 (* k (ft ft (- k 1))))))) 10)

(define (fib n)
    (define (fib-iter a b counter)
        (cond
            ((= counter 2) b)
            ((= counter 1) a)
            (else (fib-iter b (+ a b) (- counter 1)))))
    (fib-iter 0 1 n))

; fib n
((lambda (n)
    ((lambda (fib) (fib fib 0 1 n))
        (lambda (fi a b counter)
            (cond
                ((= counter 2) b)
                ((= counter 1) a)
                (else (fi fi b (+ a b) (- counter 1))))))) 10)

(define (f x)
    ((lambda (even? odd?) (even? even? odd? x))
        (lambda (ev? od? n)
            (if (= n 0) true (od? ev? od? (- n 1))))
        (lambda (ev? od? n)
            (if (= n 0) false (ev? ev? od? (- n 1))))))
(f 33)
(f 44)