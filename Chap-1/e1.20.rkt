#lang sicp

(define (gcd a b)
  (if (= b 0) a
      (gcd b (remainder a b))))

#|
(gcd 206 40)

normal-order
->(gcd 40 (remainder 206 40))
->(gcd 6[+1] (remainder 40 6))
->(gcd 4[+1] (remainder 6 4))
->(gcd 2[+1] (remainder 4 2))
->2[+1]
4-times

applicative-order
->(gcd 40 6[+1])
->(gcd 6 4[+1])
->(gcd 4 2[+1])
->(gcd 2 0[+1])
->2
4-times
|#