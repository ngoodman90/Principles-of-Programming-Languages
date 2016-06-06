#lang racket

(provide (all-defined-out))

; +-----------------------+
; |   Answer Q1-e below   |
; +-----------------------+

; Signature: part-of-set?(s,e)
; Type: Set[Numbers]*Number -> boolean
; Purpose: states if a number is part of a set or not
; Pre-conditions:
; Tests: 
(define part-of-set?
    (lambda (s e)
      (if (set-empty? s) #f (not(boolean?(memq e (set->list s))))
          )))



; empty-set primitive value
(define empty-set
  (list))

; Signature: make-set(elements)
; Type: [List(Number) -> Set(Number)]
; Purpose: Construct a set with the numbres in ‘elements’
; Pre-conditions:
; Tests:(set->list (make-set (list 1 2 3))) -> '(1 2 3)
(define make-set
  (lambda (elements)
    (lambda(sel e) (sel e elements))
    ))

; Signature: set-extend(e, s)
; Type: [Number*Set(Number) -> Set(Number)]
; Purpose: Extend set ‘s’ with the number ‘e’
; Pre-conditions:(and (set? s) (number? e))
; Tests:(set->list (set-extend 4 (make-set (list 1 2 3)))) -> '(4 1 2 3)
(define set-extend
  (lambda (e s)
    (if (part-of-set? s e) s (make-set (cons e (set->list s))))
    ))


; Signature: set-select(s)
; Type: [Set(Number) -> Number]
; Purpose: select an element from set ‘s’. 
; Pre-conditions: not(empty?(s))
; Pre-conditions:
; Tests: (set-select (make-set (list 1 2 3))) -> 1
(define set-select
  (lambda (s)
   (if (set-empty? s) s (car (set->list s))
    )))

; Signature: set-remove-el(e,s)
; Type: [Number*Set(Number) -> Set(Number)]
; Purpose: Remove 'e' from set ‘s’. If 'e' is not in 's', do nothing.
; Pre-conditions: not(empty?(s))
; Pre-conditions:
; Tests:(set->list (set-remove-el 2 (make-set (list 1 2 3)))) -> '(1 3)
(define set-remove-el
  (lambda (e s)
    (if (not(part-of-set? s e)) s (make-set (remove e (set->list s))))
    ))

; Signature: set-remove(s)
; Type: [Set(Number) -> Set(Number)]
; Purpose: Remove any element from set ‘s’. 
; Pre-conditions: not(empty?(s))
; Pre-conditions: (set? s)
; Tests:(set->list (set-remove (make-set (list 1 2 3)))) -> '(2 3)
(define set-remove
  (lambda (s)
    (set-remove-el (set-select s) s))
  )

; Signature: set->list(s)
; Type: [Set(Number) -> List(Number)]
; Purpose: Turns set ‘s’ into a list with the same elements. 
; Pre-conditions: (set? s)
; Tests:(set->list (make-set (list 1 2 3)))
(define set->list
  (lambda (s)
   (if (set-empty? s) empty-set (s append '()))
    ))
        
; Signature: set?(s)
; Type: [T -> Boolean]
; Purpose: Set identifier 
; Pre-conditions:
; Tests:(set? (make-set(list 1 2 3))) -> #t
(define set?
  (lambda (s)
    (if (not(procedure? s)) #f
    (andmap number? (set->list s)))))

; Signature: set-empty?(s)
; Type: [Set(Number) -> Boolean]
; Purpose: Tests whether 's' is empty: Return true if and only if the set ‘s’ is empty. 
; Pre-conditions:
; Tests:(set-empty? (make-set (list 1 2 3))) -> #f
(define set-empty?
  (lambda (s)
    (equal? s empty-set))
  )

; Signature: set-equal?(s1, s2)
; Type: [Set(Number) -> Boolean]
; Purpose: Return true if and only if ‘s1’ 's2' are the same set. 
; Pre-conditions:
; Tests:(set-equal? (make-set (list 1 2 3 4)) (make-set (list 1 3 2 4))) -> #t
(define set-equal?
  (lambda (s1 s2)
    (equal? (sort (set->list s1) <) (sort (set->list s2) <))
    ))