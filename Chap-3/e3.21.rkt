#lang sicp

(define (front-ptr queue)
    (car queue))
(define (rear-ptr queue)
    (cdr queue))
(define (set-front-ptr! queue item)
    (set-car! queue item))
(define (set-rear-ptr! queue item)
    (set-cdr! queue item))

(define (make-queue)
    (cons '() '()))
(define (empty-queue? queue)
    (null? (front-ptr queue)))
(define (front-queue queue)
    (if (empty-queue? queue)
        (error "Empty queue" queue)
        (car (front-ptr queue))))
(define (insert-queue! queue item)
    (let ((queue-item (cons item '())))
        (if (empty-queue? queue)
            (begin
                (set-front-ptr! queue queue-item)
                (set-rear-ptr! queue queue-item)
                queue)
            (begin
                (set-cdr! (rear-ptr queue) queue-item)
                (set-rear-ptr! queue queue-item)
                queue))))
(define (delete-queue! queue)
    (if (empty-queue? queue)
        (error "Empty queue" queue)
        (begin
            (set-front-ptr! queue (cdr (front-ptr queue)))
            queue)))

(define (print-queue queue)
    (front-ptr queue))

; tests
(define q1 (make-queue))
(insert-queue! q1 'a)
(insert-queue! q1 'b)
(delete-queue! q1)
(delete-queue! q1)

(print-queue q1)