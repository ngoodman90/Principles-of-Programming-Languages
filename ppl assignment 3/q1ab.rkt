#lang racket

(provide (all-defined-out))

; +-----------------------+
; |  Answer Q1-a.1 below  |
; +-----------------------+


; Signature: flatmap(proc seq)
; Type: [[T1 -> List(T2)] *List(T1) -> List[T2]]
; Purpose: flattens lists of lists
; Pre-conditions:
; Tests: 
(define flatmap
(lambda (proc seq)
(foldr append (list) (map proc seq))))

; Signature: permutations(s)
; Type: [Set(T) -> List(Set(T))]
; Purpose: Compute all permutations of the elements of in ‘s’
; Pre-conditions:
; Tests: 
(define permutations
 (lambda (s)
 (if (set-empty? s)
 (list empty)
 (flatmap
 (lambda (x)
 (map (lambda (p) (cons x p))
 (permutations (set-remove-el x s))))
 (set->list s)))
 ))

  

; +-----------------------+
; |  Answer Q1-a.2 below  |
; +-----------------------+


; Signature: power-set-help(list)
; Type: List(Numbers)-> List(List(Numbers))
; Purpose: computes the power set of a given set in a list representaion
; Pre-conditions:
; Tests: 
(define power-set-help
    (lambda(list)
      (if (empty? list) list
          (cons list (flatmap (lambda(x) (power-set-help (remove x list))) list )))))




; Signature: power-set(s)
; Type:Set(Numbers) -> List(List(T)) 
; Purpose:computing the power set of a set 
; Pre-conditions:(set? s)
; Tests: (power-set (make-set (list 1 2 3))) -> '((1 2 3) (2 3) (3) (2) (1 3) (1) (1 2) ())
(define power-set
  (lambda (s)
    (append (remove-duplicates (power-set-help(set->list s))) '(()) )))

; +-----------------------+
; |   Answer Q1-b below   |
; +-----------------------+


; Signature: devide-list(num strnum)
; Type: Number*Number -> List[T]
; Purpose: creates a list for an encoded mumber of the set
; Pre-conditions:
; Tests: 
(define devide-list
    (lambda(num strnum)
      (if (= (remainder num 2) 0) (list)
          (list strnum))))
    

; Signature: s->list->help(num,strnum)
; Type: Number*Number -> List[T]
; Purpose: decodes the set
; Pre-conditions:
; Tests: 
(define set->list->help
  (lambda(s strnum)
    (if (set-empty? s) (list)
    (if (< s 2) (devide-list s strnum)
        (append (devide-list s strnum) (set->list->help (quotient s 2) (+ 1 strnum) )))
    )))


; Signature: part-of-set?(s,e)
; Type: Set[Numbers]*Number -> boolean
; Purpose: states if a number is part of a set or not
; Pre-conditions:
; Tests: 
(define part-of-set?
    (lambda (s e)
      (if (set-empty? s) #f (not(boolean?(memq e (set->list s))))
          )))


; Signature: set-select-help(s,num)
; Type: Set[Numbers]*Number -> Number
; Purpose: returns a number which is a member of the set
; Pre-conditions:
; Tests: 
(define set-select-help
  (lambda(s num)
    (if (part-of-set? s num) num (set-select-help s (+ 1 num)))
    ))


; empty-set primitive value
(define empty-set
  (list))

           

; Signature: make-set(elements)
; Type: [List(Number) -> Set(Number)]
; Purpose: Construct a set with the numbres in ‘elements’
; Pre-conditions:(list?elements)
; Tests:(set->list (make-set (list 1 2 3 2 2 2)))->'(1 2 3)
(define make-set
  (lambda (elements)
    (foldr + 0
           (map (lambda(x)(expt 2 x)) (remove-duplicates elements))
           )
    ))

; Signature: set-extend(e, s)
; Type: [Number*Set(Number) -> Set(Number)]
; Purpose: Extend set ‘s’ with the number ‘e’
; Pre-conditions:(set?s)
; Tests:(set->list (set-extend 4 (make-set (list 1 2 3)))) -> '(1 2 3 4)
(define set-extend
  (lambda (e s)
    (if (part-of-set? s e) s (make-set (cons e (set->list s))))
    ))

; Signature: set-select(s)
; Type: [Set(Number) -> Number]
; Purpose: select an element from set ‘s’. 
; Pre-conditions: not(empty?(s))
; Tests:(set-select (make-set (list 1 2 3)))-> 1
(define set-select
  (lambda (s)
   (if (set-empty? s) empty-set (set-select-help s 0))
    ))
    
    

; Signature: set-remove-el(e,s)
; Type: [Number*Set(Number) -> Set(Number)]
; Purpose: Remove 'e' from set ‘s’. If 'e' is not in 's', do nothing.
; Pre-conditions: not(empty?(s))
; Tests:(set->list (set-remove-el 2 (make-set (list 1 2 3)))) -> '(1 3)
(define set-remove-el
  (lambda (e s)
    (if (not(part-of-set? s e)) s (make-set (remove e (set->list s))))
    ))
    

; Signature: set-remove(s)
; Type: [Set(Number) -> Set(Number)]
; Purpose: Remove any element from set ‘s’. 
; Pre-conditions: not(empty?(s))
; Tests:(set->list (set-remove (make-set (list 1 2 3)))) -> '(2 3)
(define set-remove
  (lambda (s)
    (set-remove-el (set-select s) s))
  )

; Signature: set->list(s)
; Type: [Set(Number) -> List(Number)]
; Purpose: Turns set ‘s’ into a list with the same elements. 
; Pre-conditions:(set? s)
; Tests:(set->list (make-set (list 1 2 3))) -> '(1 2 3)
(define set->list
  (lambda (s)
    (set->list->help s 0))
  )
        
; Signature: set?(s)
; Type: [T -> Boolean]
; Purpose: Set identifier 
; Pre-conditions:
; Tests:(set? (make-set(list 1 2 3))) -> #t(define set?
  (lambda (s)
    (or (number? s) (set-empty? s))))

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
; Pre-conditions:(and (set? s1) (set? s2))
; Tests:(set-equal? (make-set (list 1 2 3 4)) (make-set (list 1 3 2 4))) -> #t

(define set-equal?
  (lambda (s1 s2)
    (equal? (sort (set->list s1) <) (sort (set->list s2) <))
    ))