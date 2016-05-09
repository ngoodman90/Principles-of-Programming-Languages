#lang racket

(provide (all-defined-out))

; +-----------------------+
; |   Answer Q1-b below   |
; +-----------------------+

; empty-set primitive value
(define empty-set ... )

; Signature: make-set(elements)
; Type: [List(Number) -> Set(Number)]
; Purpose: Construct a set with the numbres in ‘elements’
; Pre-conditions:
; Tests:
(define make-set
  (lambda (elements)
    ...))

; Signature: set-extend(e, s)
; Type: [Number*Set(Number) -> Set(Number)]
; Purpose: Extend set ‘s’ with the number ‘e’
; Pre-conditions:
; Tests:
(define set-extend
  (lambda (e s)
    ...))

; Signature: set-select(s)
; Type: [Set(Number) -> Number]
; Purpose: select an element from set ‘s’. 
; Pre-conditions: not(empty?(s))
; Pre-conditions:
; Tests:
(define set-select
  (lambda (s)
    ...))

; Signature: set-remove-el(e,s)
; Type: [Number*Set(Number) -> Set(Number)]
; Purpose: Remove 'e' from set ‘s’. If 'e' is not in 's', do nothing.
; Pre-conditions: not(empty?(s))
; Pre-conditions:
; Tests:
(define set-remove-el
  (lambda (e s)
    ...))

; Signature: set-remove(s)
; Type: [Set(Number) -> Set(Number)]
; Purpose: Remove any element from set ‘s’. 
; Pre-conditions: not(empty?(s))
; Pre-conditions:
; Tests:
(define set-remove
  (lambda (s)
    ..))

; Signature: set->list(s)
; Type: [Set(Number) -> List(Number)]
; Purpose: Turns set ‘s’ into a list with the same elements. 
; Pre-conditions:
; Tests:
(define set->list
  (lambda (s)
    ...))
        
; Signature: set?(s)
; Type: [T -> Boolean]
; Purpose: Set identifier 
; Pre-conditions:
; Tests:
(define set?
  (lambda (s)
    ...))

; Signature: set-empty?(s)
; Type: [Set(Number) -> Boolean]
; Purpose: Tests whether 's' is empty: Return true if and only if the set ‘s’ is empty. 
; Pre-conditions:
; Tests:
(define set-empty?
  (lambda (s)
    ...))

; Signature: set-equal?(s1, s2)
; Type: [Set(Number) -> Boolean]
; Purpose: Return true if and only if ‘s1’ 's2' are the same set. 
; Pre-conditions:
; Tests:
(define set-equal?
  (lambda (s1 s2)
    ...))