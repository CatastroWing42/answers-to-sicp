#lang sicp

; If n = 5
;        TN
;      /     \
;    S(5)    TN
;           /   \ 
;         S(4)   TN
;               /   \
;             S(3)   TN
;                    /   \
;                  S(2)  S(1) 
;
; If n = 10
;        TN
;      /     \
;    S(10)    TN
;           /   \
;         S(9)   TN
;               /   \
;             S(8)   TN
;                    /   \
;                  S(7)   TN
;                        /   \
;                      S(6)   TN
;                            /   \
;                          S(5)   TN
;                                /   \
;                              S(4)   TN
;                                    /   \
;                                  S(3)   TN
;                                        /   \
;                                      S(2)  S(1)
;
; 1-bit for most frequent symbol
; (n-1) bits for least frequent symbol