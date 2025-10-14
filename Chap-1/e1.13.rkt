#lang sicp

#|
Fib(0) = 0  (fai^0 - psi^0)/sqrt(5) = 0
Fib(1) = 1  (fai^1 - psi^1)/sqrt(5) = 1

for any n >= 2:
Fib(n) = Fib(n-1) + Fib(n-2)
assume Fib(n-1) = (fai^(n-1) - psi^(n-1))/sqrt(5)
       Fib(n-2) = (fai^(n-2) - psi^(n-2))/sqrt(5)
=> Fib(n-1) = (-psi*fai^n - (-fai*psi^n))/sqrt(5)
   Fib(n-2) = (psi^2 * fai^n + fai^2 * psi^n)/sqrt(5)
=> Fib(n) = ((psi^2 - psi)fai^n + (fai^2 - fai)psi^n)/sqrt(5)
          = (psi(psi-1)fai^n + fai(fai-1)psi^n)/sqrt(5)
          = (-psi*fai*fai^n + (-fai*psi*psi^n))/sqrt(5)
          = (-psi*fai)*(fai^n - psi^n)/sqrt(5)
          = (fai^n - psi^n)/sqrt(5) ##proved
|#