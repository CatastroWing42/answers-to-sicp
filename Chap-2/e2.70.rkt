#lang sicp

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))
(define (left-branch tree)
  (car tree))
(define (right-branch tree)
  (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
    (list (symbol-leaf tree))
    (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
    (weight-leaf tree)
    (cadddr tree)))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (error "bad bit -- CHOOSE-BRANCH" bit)))
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
                (choose-branch (car bits) current-branch)))
             (if (leaf? next-branch)
                 (cons (symbol-leaf next-branch)
                       (decode-1 (cdr bits) tree))
                 (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))
(define (make-leaf-set pairs)
  (if (null? pairs) '()
    (let ((pair (car pairs)))
      (adjoin-set (make-leaf (car pair) (cadr pair))
                  (make-leaf-set (cdr pairs))))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))
(define (successive-merge leaf-set)
  (if (null? (cdr leaf-set))
      (car leaf-set)
      (let ((first (car leaf-set))
            (second (cadr leaf-set))
            (rest (cddr leaf-set)))
           (successive-merge (adjoin-set (make-code-tree first second) rest)))))

(define (encode message tree)
  (if (null? message)
      '()
      (cons (encode-symbol (car message) tree)
        (encode (cdr message) tree))))
(define (encode-symbol symbol tree)
  (define match-leaf 'match)
  (define nomatch-leaf 'nomatch)
  (define (encode-symbol-1 symbol current-branch)
    (if (leaf? current-branch)
        (if (eq? symbol (symbol-leaf current-branch))
            match-leaf nomatch-leaf)
        (let ((encoded-left (encode-symbol-1 symbol (left-branch current-branch))))
             (cond ((eq? encoded-left match-leaf) '(0))
                   ((not (eq? encoded-left nomatch-leaf)) (cons 0 encoded-left))
                   (else
                     (let ((encoded-right (encode-symbol-1 symbol (right-branch current-branch))))
                       (cond ((eq? encoded-right match-leaf) '(1))
                             ((not (eq? encoded-right nomatch-leaf)) (cons 1 encoded-right))
                             (else nomatch-leaf))))))))
  (let ((result (encode-symbol-1 symbol tree)))
    (if (eq? result nomatch-leaf)
        (error "Symbol not in tree -- ENCODE-SYMBOL" symbol)
        result)))

(define rock-song-tree
  (generate-huffman-tree
    '((A 2) (GET 2) (SHA 3) (WAH 1)
      (BOOM 1) (JOB 2) (NA 16) (YIP 9))))

(define song
  '(GET A JOB 
    SHA NA NA NA NA NA NA NA NA 
    GET A JOB 
    SHA NA NA NA NA NA NA NA NA 
    WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP 
    SHA BOOM))

(define encoded-song (encode song rock-song-tree))

(define (accumulate op initial sequence)
  (if (null? sequence) initial
    (op (car sequence) (accumulate op initial (cdr sequence)))))

(display encoded-song)
(newline)
(accumulate + 0 (map length encoded-song)); 84-bits
; If use fixed-length encoding, each symbol will consume 3-bits
; this song of total 36 symbols will consume 108 bits
