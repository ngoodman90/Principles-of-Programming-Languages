#lang racket

(require "substitution-core.rkt" "../asp.rkt")
;(require "../utils.rkt")
;(provide (all-defined-out) (all-from-out "utils.rkt"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Primitive procedures tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (primitive-tests)
  (test (derive-eval '(* 3 4)) => '(value 12))
  (test (derive-eval '(+ 3 4)) => '(value 7))
  (test (derive-eval '(- 3 4)) => '(value -1))
  (test (derive-eval '(/ 4 2)) => '(value 2))
  (test (derive-eval '(null? (list))) => '(value #t))
  (test (derive-eval '(> 3 4)) => '(value #f))
  (test (derive-eval '(< 3 4)) => '(value #t))
  (test (derive-eval '(= 3 4)) => '(value #f))
  (test (derive-eval '(car (list 3 4))) => '(value 3))
  (test (derive-eval '(cdr (list 3 4))) => '(value (4)))
  (test (derive-eval '(cons 3 (cons 4 (list)))) => '(value (3 4)))
  (test (derive-eval-no-value-tag '(cons 3 (cons 4 (list)))) => '(3 4))  
  (test (derive-eval '(list 3 4)) => '(value (3 4)))
  ;; '((map (lambda (x) x) (list 1 2 3)) (1 2 3))
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Application and `lambda' tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (app-lambda-tests)
  (test (derive-eval '((lambda (x) x) 12)) => '(value 12))
  (test (derive-eval '((lambda (x y z) (+ x y z)) 12 13 14)) => '(value 39))
  (test (derive-eval '((lambda (x) (+ x 1)) 2)) => '(value 3))
  (test (derive-eval '((lambda (x) ((lambda (x) (+ x 1)) 2)) 12)) => '(value 3))
  (test (derive-eval '((lambda (f) (f 2 1)) +)) => '(value 3))
  (test (derive-eval '((lambda (f x y) (f x y)) + 12 4)) => '(value 16))
  (test (derive-eval '((lambda (f x y) (f x y)) ((lambda () +)) 12 4)) => '(value 16))
  (test (derive-eval '((lambda (f) (f 4)) (lambda (x) (+ x x)))) => '(value 8))
  (test (derive-eval '((lambda (g) (g)) (lambda () (* 3 2)))) => '(value 6))
  (test (derive-eval '((lambda (x) x) ((lambda (x) x) 6))) => '(value 6))
  (test (derive-eval '((lambda (f g) (f (g))) (lambda (x) x) (lambda () (* 3 2)))) => '(value 6))
  (test (derive-eval '((lambda (lst) (car lst)) (list 11 12 13))) => '(value 11))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `begin' tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (begin-tests)
  (test (derive-eval '(begin 1 2 3)) => '(value 3))
  (test (derive-eval '(begin 1 2 ((lambda () 5)))) => '(value 5)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `let' tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (let-tests)
  (test (derive-eval '(let ((x 1) (y 2)) (+ x y))) => '(value 3))
  (test (derive-eval '(let ((x 1) (y 2)) (+ x y ((lambda (x) (* 2 x)) 10)))) => '(value 23))
  (test (derive-eval '(let ((x ((lambda () 5)))) x)) => '(value 5))
  (test (derive-eval '(let ((f (lambda (x) (+ x x)))) (f 4))) => '(value 8))
  (test (derive-eval '(let ((f (lambda (x) (+ x x))) (g (lambda (x) (* x 2)))) (f (g 3)))) => '(value 12))
  (test (derive-eval '(let ((f (lambda (x) (+ x x))) (g (lambda (x y) (* x y))) (h (lambda () 2))) (f (g 3 (h))))) => '(value 12)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; definition and function-definition tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (definition-tests)
  (test (derive-eval '(define x 2)) => 'ok)
  (test (derive-eval '(define (f x) (+ x x))) => 'ok)
  (test (derive-eval 'x) => '(value 2))
  (test (derive-eval '(f x)) => '(value 4))
 ; applications that require renaming: 
  (test (derive-eval '(define h (lambda (f)(lambda (x) (f x))))) => 'ok)
  (test (derive-eval-no-value-tag '(define h1 (lambda (f)(lambda (x) (f x))))) => 'ok)
  (test (derive-eval '(define g (lambda (y) (+ x y)))) => 'ok)
  (test (derive-eval '((h1 g) 3)) => '(value 5))  ; without renaming the result is 6
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `if' tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (if-tests)
  (test (derive-eval '(if (> 3 2) 5 1)) => '(value 5))
  (test (derive-eval '(if (< 3 2) 5 1)) => '(value 1))
  (test (derive-eval '(if (> (+ 1 2) 2) (+ 1 2 3) 1)) => '(value 6))
  (test (derive-eval '(define (f1 n) (if (= n 0) 1 (* n (f (- n 1)))))) => 'ok)
  (test (derive-eval '(define (f2 n) (if (= n 0) 1 (* n (f2 (- n 1)))))) => 'ok)
  (test (derive-eval '(f1 5)) => '(value 40))
  (test (derive-eval '(f2 5)) => '(value 120)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `cond' tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (cond-tests)
  (test (derive-eval '(cond ((> 3 2) 5) (else 1))) => '(value 5))
  (test (derive-eval '(cond ((< 3 2) 5) (else 1))) => '(value 1))
  (test (derive-eval '(let ((x 3)) (cond ((= x 1) 10) ((= x 2) 20) ((= x 3) 30) (else 100)))) => '(value 30))
  (test (derive-eval '(let ((x 10)) (cond ((= x 1) 10) ((= x 2) 20) ((= x 3) 30) (else 100)))) => '(value 100)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Y-combinator tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (y-comb-tests)
  (test (derive-eval '(( (lambda (f) (f f))
                         (lambda (fact)
                           (lambda (n)
                             (if (= n 0)
                                 1
                                 (* n ((fact fact) (- n 1)))))))
                       6))
        => '(value 720)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Invoking tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(run-tests
 (primitive-tests)
 (app-lambda-tests)
 (begin-tests)
 (let-tests)
 (app-lambda-tests)
 (begin-tests)
 (let-tests)
 (definition-tests)
 (if-tests)
 (cond-tests)
 (y-comb-tests)
 )
