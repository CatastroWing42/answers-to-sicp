#lang sicp

(define (memo-proc proc)
    (let ((already-run? false) (run-result false))
        (lambda ()
        (if (not already-run?)
            (begin
                (set! run-result (proc))
                (set! already-run? true)
                run-result)
            run-result))))
(define-syntax delay
    (syntax-rules ()
        ((delay exp) (memo-proc (lambda () exp)))))
; (define (delay exp)
;     (memo-proc
;         (lambda () exp)))
(define (force d) (d))

(define-syntax cons-stream
    (syntax-rules ()
        ((cons-stream a b) (cons a (delay b)))))
; (define (cons-stream a b)
;     (cons a (delay b)))
(define (stream-car s)
    (car s))
(define (stream-cdr s)
    (force (cdr s)))
(define the-empty-stream '())
(define (stream-null? s) (null? s))

(define (stream-ref s n)
    (if (= n 0)
        (stream-car s)
        (stream-ref (stream-cdr s) (- n 1))))

(define (stream-ref-rest s n)
    (if (= n 0)
        s
        (stream-ref-rest (stream-cdr s) (- n 1))))

(define (stream-map proc . argstreams)
    (if (stream-null? (car argstreams))
        the-empty-stream
        (cons-stream
            (apply proc (map stream-car argstreams))
            (apply stream-map
                (cons proc (map stream-cdr argstreams))))))

(define (display-line x)
    (newline) (display x))

(define (show x)
    (display-line x)
    x)

(define (stream-enumerate-interval low high)
    (if (> low high)
        the-empty-stream
        (cons-stream low
            (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred stream)
    (cond
        ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
            (cons-stream (stream-car stream)
                (stream-filter pred (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (stream-for-each proc s)
    (if (not (stream-null? s))
        (begin
            (proc (stream-car s))
            (stream-for-each proc (stream-cdr s)))))
(define (stream-for-each-n proc s n)
    (if (and (not (stream-null? s))
            (> n 0))
        (begin
            (proc (stream-car s))
            (stream-for-each-n proc (stream-cdr s) (- n 1)))))
(define (display-stream s)
    (stream-for-each display-line s))
(define (display-stream-n s n)
    (stream-for-each-n display-line s n))
(define (add-streams s1 s2)
    (if (stream-null? s1)
        the-empty-stream
        (cons-stream (+ (stream-car s1) (stream-car s2))
            (add-streams (stream-cdr s1) (stream-cdr s2)))))
(define (mul-streams s1 s2)
    (if (stream-null? s1)
        the-empty-stream
        (cons-stream (* (stream-car s1) (stream-car s2))
            (mul-streams (stream-cdr s1) (stream-cdr s2)))))

(define ones
    (cons-stream 1 ones))

(define integers
    (cons-stream 0 (add-streams integers ones)))
(define positive-integers
    (stream-ref-rest integers 1))
; (stream-ref integers 3)
; (stream-ref positive-integers 3)
; (stream-ref positive-integers 4)

(define factorials
    (cons-stream 1 (mul-streams (stream-ref-rest integers 2)
                        factorials)))
; (stream-ref factorials 3)
; (stream-ref factorials 5)

; 3.55
(define (partial-sums s)
    (cons-stream (stream-ref s 0)
        (add-streams (partial-sums s)
            (stream-ref-rest s 1))))

(define partial-sums-positive
    (partial-sums positive-integers))
; (stream-ref partial-sums-positive 0)
; (stream-ref partial-sums-positive 1)
; (stream-ref partial-sums-positive 2)
; (stream-ref partial-sums-positive 3)
; (stream-ref partial-sums-positive 4)

; 3.56
(define (merge s1 s2)
    (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else (let ((e1 (stream-car s1))
                    (e2 (stream-car s2)))
                (cond
                    ((> e1 e2)
                        (cons-stream e2
                            (merge s1 (stream-cdr s2))))
                    ((< e1 e2)
                        (cons-stream e1
                            (merge (stream-cdr s1) s2)))
                    (else
                        (cons-stream e1
                            (merge (stream-cdr s1) (stream-cdr s2)))))))))
(define (scale-stream s factor)
    (if (stream-null? s)
        the-empty-stream
        (cons-stream
            (* (stream-car s) factor)
            (scale-stream (stream-cdr s) factor))))

(define S
    (cons-stream 1 (merge
                    (merge (scale-stream S 2) (scale-stream S 3))
                    (scale-stream S 5))))
; (display-stream-n S 40)

; 3.57
(define fib
    (cons-stream 0
        (cons-stream 1
            (add-streams fib (stream-cdr fib)))))
; (display-stream-n fib 10)
; O(n) if use memoized proc
; exp(n) if not

; 3.58
; its a ring num which next num = (radix * num) mod den
(define (split-line)
    (newline)
    (display "----------------------------")
    (newline))

(define (expand num den radix)
    (cons-stream
        (quotient (* num radix) den)
        (expand (remainder (* num radix) den) den radix)))
(define e1 (expand 1 7 10))
(define e2 (expand 3 8 10))
; (display-stream-n e1 10)
; (split-line)
; (display-stream-n e1 10)

; 3.59
(define (reciprocal s)
    (if (stream-null? s)
        the-empty-stream
        (cons-stream (/ 1 (stream-car s))
            (reciprocal (stream-cdr s)))))

(define (integrate-series s)
    (mul-streams s
        (reciprocal positive-integers)))

(define exp-series
    (cons-stream 1
        (integrate-series exp-series)))

; (split-line)
; (display-stream-n exp-series 10)

(define sine-series
    (cons-stream 0 (integrate-series cosine-series)))
(define cosine-series
    (cons-stream 1 (scale-stream (integrate-series sine-series) -1)))

; (split-line)
; (display-stream-n sine-series 10)
; (split-line)
; (display-stream-n cosine-series 10)

; 3.60
(define (mul-series s1 s2)
    (cons-stream (* (stream-car s1) (stream-car s2))
        (add-streams (cons-stream 0 (mul-series (stream-cdr s1) (stream-cdr s2)))
            (add-streams (scale-stream (stream-cdr s1) (stream-car s2))
                (scale-stream (stream-cdr s2) (stream-car s1))))))
(define square-sin-plus-square-cos
    (add-streams (mul-series sine-series sine-series)
        (mul-series cosine-series cosine-series)))

; (split-line)
; (display-stream-n square-sin-plus-square-cos 30)

; 3.61
(define (invert-unit-series S)
    (if (= (stream-car S) 1)
        (cons-stream 1
            (scale-stream (mul-series (stream-cdr S) (invert-unit-series S))
                -1))
        (error "Invalid Series: INVERT-UNIT-SERIES")))

; test: 1 + x + x^2 + x^3 + ... = 1/(1-x)
; 其逆应该是 1 - x
; (define test
;     (cons-stream 1 test))
; (define invert-test
;     (invert-unit-series test))
; (display-stream-n invert-test 10)

; 3.62
(define (div-series s1 s2)
    (if (= (stream-car s2) 0)
        (error "Invalid Denominator")
        (let ((b0 (stream-car s2)))
            (mul-series s1
                (scale-stream (invert-unit-series
                                (scale-stream s2 (/ 1 b0)))
                            b0)))))

(define tangent-series
    (div-series sine-series cosine-series))
(display-stream-n tangent-series 10)

; 3.64
(define (stream-limit s tolerance)
    (let ((a (stream-car s))
            (b (stream-car (stream-cdr s))))
        (if (< (abs (- a b)) tolerance)
            b
            (stream-limit (stream-cdr s) tolerance))))

(define (average a b)
    (/ (+ a b) 2))
(define (sqrt-improve guess x)
    (average guess (/ x guess)))
(define (sqrt-stream x)
    (define guesses
        (cons-stream 1.0
            (stream-map (lambda (guess) (sqrt-improve guess x))
                guesses)))
    guesses)

(define (sqrt x tolerance)
    (stream-limit (sqrt-stream x) tolerance))

(split-line)
(sqrt 2 0.001)

; 3.65
(define (ln2-summands n)
    (cons-stream (/ 1.0 n)
        (stream-map - (ln2-summands (+ n 1)))))
(define ln2-stream
    (partial-sums (ln2-summands 1)))

(split-line)
(display-stream-n ln2-stream 100)

(define (square x) (* x x))
(define (euler-transform s)
    (let ((s0 (stream-ref s 0))
            (s1 (stream-ref s 1))
            (s2 (stream-ref s 2)))
        (cons-stream (- s2 (/ (square (- s2 s1))
                                (+ s0 (* -2 s1) s2)))
                    (euler-transform (stream-cdr s)))))

(split-line)
(display-stream-n (euler-transform ln2-stream) 100)

(define (make-tableau transform s)
    (cons-stream s (make-tableau transform (transform s))))
(define (accelerated-sequence transform s)
    (stream-map stream-car (make-tableau transform s)))

(split-line)
(display-stream-n (accelerated-sequence euler-transform ln2-stream) 100)

; 3.68
(define (interleave s1 s2)
    (if (stream-null? s1)
        s2
        (cons-stream (stream-car s1)
            (interleave s2 (stream-cdr s1)))))
; pairs not protected by delay(cons-stream)
; program will continually call pairs function to compute second interleave stream
; (define (pairs s t)
;     (interleave
;         (stream-map (lambda (x) (list (stream-car s) x))
;             t)
;         (pairs (stream-cdr s) (stream-cdr t))))
; 
; (split-line)
; (display-stream-n
;     (pairs positive-integers positive-integers) 20) ; this will loop forever

; 3.69
(define (pairs s t)
    (cons-stream
        (list (stream-car s) (stream-car t))
        (interleave
            (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
            (pairs (stream-cdr s) (stream-cdr t)))))

(define (interleave-3 s1 s2 s3)
    (if (stream-null? s1)
        (interleave s2 s3)
        (cons-stream (stream-car s1)
            (interleave-3 s2 s3 s1))))
(define (triples s t u)
    (cons-stream
        (list (stream-car s) (stream-car t) (stream-car u))
        (interleave
            (stream-map (lambda (x) (cons (stream-car s) x))
                (stream-cdr (pairs t u)))
            (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))

; (split-line)
; (display-stream-n
;     (triples positive-integers positive-integers positive-integers) 20)
(define pythagorean-stream
    (stream-filter
        (lambda (x)
            (= (+ (square (car x)) (square (cadr x)))
                (square (caddr x))))
        (triples positive-integers positive-integers positive-integers)))

; (split-line)
; (display-stream-n pythagorean-stream 20)

; 3.70
(define (weighted-merge s1 s2 weight)
    (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else (let ((e1 (stream-car s1))
                    (e2 (stream-car s2)))
                (let ((we1 (weight e1)) (we2 (weight e2)))
                    (cond
                        ((> we1 we2)
                            (cons-stream e2
                                (weighted-merge s1 (stream-cdr s2) weight)))
                        (else
                            (cons-stream e1
                                (weighted-merge (stream-cdr s1) s2 weight)))))))))
(define (weighted-pairs s t weight)
    (cons-stream
        (list (stream-car s) (stream-car t))
        (weighted-merge
            (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
            (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
            weight)))

; (split-line)
; (display-stream-n
;     (weighted-pairs
;         positive-integers positive-integers
;         (lambda (x) (+ (car x) (cadr x)))) 20)
; 
; (split-line)
; (display-stream-n
;     (stream-filter
;         (lambda (x)
;             (define (check a)
;                 (and (> (remainder a 2) 0) (> (remainder a 3) 0) (> (remainder a 5) 0)))
;             (and (check (car x)) (check (cadr x))))
;         (weighted-pairs
;             positive-integers positive-integers
;             (lambda (x) (+ (* 2 (car x)) (* 3 (cadr x)) (* 5 (car x) (cadr x)))))) 20)

; 3.71
(define (weighted-same-stream s weight)
    (if (stream-null? s)
        the-empty-stream
        (if (= (weight (stream-car s))
                (weight (stream-car (stream-cdr s))))
            (cons-stream
                (weight (stream-car s))
                (weighted-same-stream (stream-cdr (stream-cdr s)) weight))
            (weighted-same-stream (stream-cdr s) weight))))
(define (unique-stream s)
    (if (stream-null? s)
        the-empty-stream
        (if (= (stream-car s) (stream-car (stream-cdr s)))
            (unique-stream (stream-cdr s))
            (cons-stream
                (stream-car s)
                (unique-stream (stream-cdr s))))))
(define (cube x) (* x x x))
(define sum-of-cubes
    (lambda (x)
        (+ (cube (car x))
            (cube (cadr x)))))
(define Ramanujan-stream
    (unique-stream
        (weighted-same-stream
            (weighted-pairs
                positive-integers positive-integers
                sum-of-cubes) sum-of-cubes)))

(split-line)
(display-stream-n Ramanujan-stream 6)

; 3.72
(define (sum-of-squares x)
    (+ (square (car x)) (square (cadr x))))
(define (stream-same-3 s)
    (let ((e1 (stream-car s))
            (e2 (stream-car (stream-cdr s)))
            (e3 (stream-car (stream-cdr (stream-cdr s))))
            (e4 (stream-car (stream-cdr (stream-cdr (stream-cdr s))))))
        (if (and
                (= (car e1) (car e2))
                (= (car e2) (car e3))
                (not (= (car e3) (car e4))))
            (cons-stream
                (list (car e1) (cdr e1) (cdr e2) (cdr e3))
                (stream-same-3
                    (stream-cdr (stream-cdr (stream-cdr s)))))
            (stream-same-3
                (stream-cdr (stream-cdr (stream-cdr s)))))))
(define sum-of-squares-in-three-ways
    (stream-same-3
        (stream-map
            (lambda (x) (cons (sum-of-squares x) x))
            (weighted-pairs
                positive-integers positive-integers
                sum-of-squares))))

(split-line)
(display-stream-n sum-of-squares-in-three-ways 10)

; 3.73
(define (integral integrand initial-value dt)
    (define int
        (cons-stream
            initial-value
                (add-streams (scale-stream integrand dt)
                    int)))
    int)

(define (RC r c dt)
    (lambda (i-stream v0)
        (add-streams
            (integral (scale-stream i-stream (/ 1.0 c))
                v0 dt)
            (scale-stream i-stream r))))

(split-line)
(define RC1 (RC 5 1 0.5))
(display-stream-n (RC1 ones 0) 10)

; 3.74
; (define zero-crossings
;     (stream-map sign-change-detector
;         sense-data
;         (cons-stream 0
;             sense-data)))

; 3.75
; (define (make-zero-crossing input-stream last-avpt last-value)
;     (let ((avpt (/ (+ (stream-car input-stream)
;                         last-value)
;                     2)))
;         (cons-stream
;             (sign-change-detector avpt last-avpt)
;             (make-zero-crossing
;                 (stream-cdr input-stream)
;                 avpt (stream-car input-stream)))))

; 3.76
(define (smooth s)
    (define (make-smooth s last-value)
        (let ((avpt (/ (+ (stream-car s)
                            last-value)
                        2)))
            (cons-stream avpt
                (make-smooth
                    (stream-cdr s)
                    (stream-car s)))))
    (cons-stream (stream-car s)
        (make-smooth (stream-cdr s) (stream-car s))))
; (define zero-crossings
;     (make-zero-crossing
;         (smooth sense-data) 0))

(define (delayed-integral delayed-integrand initial-value dt)
    (define int
        (cons-stream
            initial-value
            (let ((integrand (force delayed-integrand)))
                (add-streams (scale-stream integrand dt) int))))
    int)
(define (solve f y0 dt)
    (define y (delayed-integral (delay (stream-map f y)) y0 dt))
    y)

(split-line)
(display (stream-ref (solve (lambda (x) x) 1 0.001) 1000))

; 3.77
(define (delayed-integral-v2 delayed-integrand initial-value dt)
    (cons-stream
        initial-value
        (let ((integrand (force delayed-integrand)))
            (if (stream-null? integrand)
                the-empty-stream
                (delayed-integral-v2 (delay (stream-cdr integrand))
                    (+ (* dt (stream-car integrand))
                        initial-value)
                    dt)))))
(define (solve-v2 f y0 dt)
    (define y (delayed-integral-v2 (delay (stream-map f y)) y0 dt))
    y)

(split-line)
(display (stream-ref (solve-v2 (lambda (x) x) 1 0.001) 1000))

; 3.78
(define (solve-2nd a b dt y0 dy0)
    (define ddy
        (add-streams (scale-stream dy a)
            (scale-stream y b)))
    (define dy (delayed-integral (delay ddy) dy0 dt))
    (define y (delayed-integral (delay dy) y0 dt))
    y)

; 3.79
(define (general-solve-2nd f dt y0 dy0)
    (define ddy
        (stream-map f dy y))
    (define dy (delayed-integral (delay ddy) dy0 dt))
    (define y (delayed-integral (delay dy) y0 dt))
    y)

; 3.80
(define (RLC r l c dt)
    (lambda (vc0 il0)
        (define vc
            (delayed-integral
                (delay (scale-stream il (/ -1.0 c)))
                vc0 dt))
        (define il
            (delayed-integral
                (delay (add-streams
                    (scale-stream vc (/ 1.0 l))
                    (scale-stream il (/ (* -1.0 r) l))))
                il0 dt))
        (stream-map cons vc il)))

(split-line)
(define RLC1 (RLC 1 1 0.2 0.1))
(display-stream-n (RLC1 10 0) 10)