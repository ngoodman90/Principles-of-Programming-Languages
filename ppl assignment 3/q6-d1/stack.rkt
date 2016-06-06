#lang racket
(provide (all-defined-out))

;; Stack ADT
;;
;; A stack implemented as a list of items.
;;
;; Constructors:
;; Signature: make-stack() -> returns an empty stack
;;            stack-push(stack item) -> returns an extended stack
;;            stack-pop(stack) -> returns a stack without top
;; Type:
;;  Client view: [Stack * T -> Stack]
;;  Supplier view: [LIST * T -> LIST]
;; Example:
;;  (stack-push (make-stack) 1) ==> (1)
;;  (stack-pop (stack-push (make-stack) 1)) ==> '()

;; Invariants:
;; (stack-pop (stack-push s i)) == s
;; (stack->top (stack-push s i)) == i

(define make-stack
  (lambda () (list)))

(define stack-push
  (lambda (stack item)
    (cons item stack)))

(define stack-pop cdr)

;; Membership predicate
(define stack? list?)
(define stack-empty? empty?)

;; Selector
;; Signature: stack->top(stack)
;; Type: Client view: [Stack -> T]
;;       Supplier view: [LIST -> T]
(define stack->top car)

(define stack-length length)
