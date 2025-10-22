#lang sicp

; Changes for a system new type must open be added:
; 1. generic operations with explicit dispatch:
;    - every generic operation
;    - new type specific things
; 2. data-directed programming:
;    - new type things
; 3. message-passing: --> <least change>
;    - new type things

; Changes for a system new operation must open be added:
; 1. generic operations with explicit dispatch: --> <best choice>
;    - new generic operation procedure
; 2. data-directed programming:
;    - new operation for each data type
; 3. message-passing:
;    - new operation for each data type