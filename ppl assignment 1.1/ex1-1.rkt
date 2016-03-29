#lang racket
(provide (all-defined-out))
;; DO NOT EDIT ABOVE THIS LINE ;;


;;;;;;;;;;;;;;;;
;; Question 6 ;;
;;;;;;;;;;;;;;;;

; Signature: list-reverse
; Type:[list -> list]
; Purpose: Reverses a given list
; Pre-conditions: lst is a scheme type list
; Tests: (test (list-reverse '(1 2 3)) => '(3 2 1))
(define list-reverse
  (lambda (lst)
    (list-reverse-helper lst '())
    ))

; Signature: list-reverse-helper
; Type: [list*list -> list]
; Purpose:Takes the first part of the list and adds it to the end of second list, recursively reversing the list
; Pre-conditions:
; Tests:(test (list-reverse-helper '(1 2 3) '()) => '(3 2 1))
(define list-reverse-helper
  (lambda (lst1 lst2)
    (if (null? lst1) lst2
          (list-reverse-helper (cdr lst1) (cons (car lst1) lst2)))))

;;;;;;;;;;;;;;;;
;; Question 7 ;;
;;;;;;;;;;;;;;;;

; Signature: approx-sqrt2
; Type:[number -> number]
; Purpose:approximates the square of 2
; Pre-conditions:n is a number
; Tests:(test (approx-sqrt2 1) => 1.3333333333333333)
;       (test (approx-sqrt2 10) => 1.4142135731001355)
(define approx-sqrt2
  (lambda (n)
    (approx-sqrt2-helper n 1.0)))
    
; Signature: approx-sqrt2-helper
; Type:[number*number -> number]
; Purpose: helps approximate sqrt2 iteratively
; Pre-conditions:n and acc are numbers
; Tests:(test (approx-sqrt2-helper 1 1.0) => 1.3333333333333333)
;       (test (approx-sqrt2-helper 10 1.0) => 1.4142135731001355)
(define approx-sqrt2-helper
  (lambda (n acc)
    (cond ((= n 0) (+ 1 (/ 1 acc)))
          (else (approx-sqrt2-helper (- n 1) (+ 2.0 (/ 1 acc)))))))
