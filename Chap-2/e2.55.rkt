#lang sicp

''abcccccdfda
(car ''abcccccdfda)

;; (list 'car (list 'quote 'abcccccdfda))
;; 'abcccccdfda is eq to (quote abcccccdfda)
;; => ''abcccccdfda equals to '(quote abcccccdfda) aka (list 'quote 'abcccccdfda)
;; so (car ''abcccccdfda) is equivalent to (car (list 'quote 'abcccccdfda)) => quote