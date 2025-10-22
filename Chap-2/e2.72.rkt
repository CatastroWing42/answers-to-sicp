#lang sicp

; Consider that of e2.71
; let Steps(n) = 1*2^(n-1)+2*2^(n-2)...+(n-1)*2^1+(n-1)
; Evident to prove that: Steps(n) = 2^(n+1)-n-3
; So, Theta(2^n)