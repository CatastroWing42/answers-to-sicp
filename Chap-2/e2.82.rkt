#lang sicp

(define (apply-generic op . args)
  (define (apply-generic-inner op args type-tags)
    (define (apply-generic-1 coerced-args)
    (let ((type-tags (map type-tag coerced-args)))
        (let ((proc (get op type-tags)))
        (if proc
            (apply proc (map contents coerced-args))
            nil))))
    (define (complete? coerced-list)
      (= (accumulate (lambda (x y) (if (null? x) (+ y 1) y)) 0 coerced-args) 0))
    (define (apply-generic-type type-tag)
    (if (null? type-tag) nil
        (let ((target-type (car type-tag)))
        (let ((coerces (map (lambda (x)
                                (if (eq? x target-type)
                                    identity
                                    (get-coercion x target-type)))
                            type-tags)))
            (let ((coerced-list (accumulate-n (lambda (x y)
                                                (cond ((null? y) x)
                                                    (x (x y))
                                                    (else nil)))
                                            nil (list coerces args))))
            (let ((apply-result (apply-generic-1 coerced-list)))
                (if (and (complete? coerced-list) apply-result)
                    apply-result
                    (apply-generic-type (cdr type-tags)))))))))
    (let ((result (apply-generic-type type-tags)))
      (if (null? result)
          (error "No method for these types" (list op type-tags))
          result)))
  (apply-generic-inner op args (map type-tag args)))

(define (identity x) x)
(define (accumulate op initial sequence)
  (if (null? sequence) initial
    (op (car sequence) (accumulate op initial (cdr sequence)))))
(define (accumulate-n op init seqs)
  (if (null? (car seqs)) nil
    (cons (accumulate op init (map car seqs))
          (accumulate-n op init (map cdr seqs)))))

; consider the tower types
;       complex
;         |
;        real
;         |
;       rational
;         |
;        integer
; if args are of type integer and real, they cannot be coerced
; directly (ie. a mixed-type coercion is needed which changes integer->rational and then rational->real)