#lang racket                   ;;
(provide (all-defined-out))    ;;
;; DO NOT EDIT ABOVE THIS LINE ;;

;;;;;;;;;;;;;;;;
;; Question 2 ;;
;;;;;;;;;;;;;;;;

; Signature: heads-rec
; Type:[list-> list]
; Purpose: return the first element of each sublist in lst recursively
; Pre-conditions:lst is list
; Tests:(test ((heads-rec '((#f 4) (b 1) (7 7 7 8))) => '(#f b 7))
(define heads-rec
  (lambda (lst)
    (flatten (heads-rec-helper (filter list? lst)))
    ))

; Signature: heads-rec-helper
; Type:[list-> list]
; Purpose:return the first element of each sublist in lst recursively
; Pre-conditions:lst is list
; Tests:
(define heads-rec-helper
  (lambda (lst)
    (if (not (empty? (cdr lst))) (list  (caar lst) (heads-rec-helper (cdr lst))) (caar lst))))

; Signature: heads-iter
; Type:[list-> list]
; Purpose: return the first element of each sublist in lst iteratively
; Pre-conditions:lst is list
; Tests:(test (heads-iter '((3 4) (1 1))) => '(3 1))
(define heads-iter
  (lambda (lst)
     (heads-iter-helper (filter list? lst) '())
    ))

; Signature: heads-iter-helper
; Type:[list-> list]
; Purpose:return the first element of each sublist in lst iteratively
; Pre-conditions:lst is list
; Tests:
(define heads-iter-helper
  (lambda (lst ans)
    (if (null? lst) ans (heads-iter-helper (cdr lst) (append ans (list (caar lst)))))))

;;;;;;;;;;;;;;;;
;; Question 3 ;;
;;;;;;;;;;;;;;;;

; Signature: compose
; Type:[list*T1 -> T2] 
; Purpose:takes as parameters a list of functions, and returns the result of applying the composition of all the functions on x
; Pre-conditions:all functions in list receive the type from the previous function in list, and return a type for their following
; Tests:(test (compose (list (lambda (x) (+ x 1))(lambda (x) (* x x)))5) => 26))
;       (test (compose (list (lambda (x) (* x x)) (lambda (x) (+ x 1)))5) => 36)) 
(define compose
  (lambda (fun-list x)
    (compose-helper (reverse fun-list) x)
    ))

; Signature: compose-helper
; Type:[list*T1 -> T2] 
; Purpose:takes as parameters a list of functions, and returns the result of applying the composition of all the functions on x
; Pre-conditions:all functions in list receive the type from the previous function in list, and return a type for their following
; Tests:
(define compose-helper
  (lambda (lst x)
    (if (empty? lst) x (compose-helper (cdr lst) ((car lst) x)))))

;;;;;;;;;;;;;;;;
;; Question 4 ;;
;;;;;;;;;;;;;;;;

; Signature: iter-tree
; Type:[list -> list]
; Purpose:iterates over a tree in certain order
; Pre-conditions:tree is a list, iter is either in-iter or pre-iter
; Tests:(test (iter-tree '(1 (#f 21 a) (3 b 32)) pre-iter) => '(1 #f 21 a 3 b 32))
;       (test (iter-tree '(1 (#f 21 a) (3 b 32)) in-iter) => '(21 #f a 1 b 3 32))
(define iter-tree
  (lambda (tree iter)
    (flatten(iter tree))
    ))

; Signature: pre-iter
; Type:[list -> list]
; Purpose:iterates over tree pre order
; Pre-conditions:tree is a list
; Tests:
(define pre-iter
  (lambda (tree)
    (cond ((null? tree) '())
          ((not (pair? tree)) tree)
          (else (append (list (car tree)
                                 (pre-iter (cadr tree))
                                 (pre-iter (caddr tree))))))
    ))

; Signature: in-iter
; Type:[list -> list]
; Purpose:iterates over tree in order
; Pre-conditions:tree is a list
; Tests:
(define in-iter
  (lambda (tree)
    (cond ((null? tree) '())
          ((not (pair? tree)) tree)
          (else (append (list (in-iter (cadr tree))
                              (car tree)
                              (in-iter (caddr tree))))))
    ))
