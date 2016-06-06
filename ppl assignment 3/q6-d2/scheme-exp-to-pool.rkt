#lang racket

(require "scheme-ast.rkt" "utils.rkt")
(provide (all-defined-out))

;; Generate a new symbol of the form T_i at each invocation
(define fresh-name
  (let((counter 0))
    (lambda name
      (if (null? name)
          (fresh-name 'T)
          (let ((new-name (string->symbol
                           (string-append (symbol->string (car name))
                                          "_" (number->string counter)))))
            (set! counter (+ 1 counter))
            new-name)))))

;; ============================================================n
;; Pool ADT
;; A pool represents a map from Scheme-exp to Type-expressions.
;; It is implemented as a list of pairs (Scheme-exp TE).
;; When a new Scheme-exp is added to a pool, a fresh Type-variable
;; is allocated for it.

;; Type: [Empty -> Pool]
(define make-empty-pool
  (lambda () '()))

(define empty-pool? empty?)

;; Purpose: construct a pool with one additional pair
;;          (scheme-exp fresh-type-var)
;; Type: [Scheme-exp * Pool -> Pool]
;; Precondition: Scheme-exp is not already in pool.
(define extend-pool
  (lambda (scheme-exp pool)
    (cons (list scheme-exp (fresh-name)) pool)))

;; Type: [Scheme-exp * Pool -> Boolean]
(define element-of-pool?
  (lambda(exp pool)
    (ormap (lambda (pool-pair) (equal? (car pool-pair) exp)) pool)))

(define pool->exp
  (lambda (pool-elt) (first pool-elt)))
(define pool->tvar
  (lambda (pool-elt) (second pool-elt)))

;; Map a function over a list of expressions to accumulate
;; matching sub-expressions into a pool.
;; Signature: map-pool(fun, exp-list, result)
;; Type: [(Scheme-exp * Pool -> Pool) * List(Scheme-exp) * Pool -> Pool]
;; fun should construct a new pool given a new expression from exp-list
;; that has not yet been seen before.
(define map-pool
  (lambda (fun exp-list result)
    (if (null? exp-list)
        result
        (map-pool fun
                  (cdr exp-list)
                  (if (element-of-pool? (car exp-list) result)
                      result
                      (fun (car exp-list) result))))))

;; Is Scheme expression of the form (quote s)
(define scheme-quoted?
  (lambda (exp)
    (and (scheme-app? exp)
         (eq? (scheme-app->operator exp) 'quote))))

;; Purpose: given a scheme exp (quote s) return s
;; Precondition: (scheme-quoted? exp)
(define scheme-quoted-var
  (lambda (exp)
    (car (scheme-app->operands exp))))

;; Type: [Scheme-exp -> Pool]
;; Purpose: Traverse the abstract syntax tree Scheme-exp
;;          and collect all sub-expressions into a Pool of fresh type variables.
;; Example:
;; (scheme-exp->pool (scheme-parse '((lambda (x) (+ x 1)) 2)))
;; ==> '(((app (lambda (x) ((app + (x 1)))) (2)) T_6)
;;       (2 T_5)
;;       ((lambda (x) ((app + (x 1)))) T_4)
;;       ((app + (x 1)) T_3)
;;       (1 T_2)
;;       (x T_1)
;;       (+ T_0))
;;
;; (scheme-exp->pool (scheme-parse '((lambda (x) x) 'a)))
;; ==> '(((app (lambda (x) (x)) ((app quote (a)))) T_3)
;;       (a T_2)
;;       ((lambda (x) (x)) T_1)
;;       (x T_0))

(define scheme-exp->pool
  (lambda (exp)
    (letrec
        ((findVarList
          (lambda (exp var-pool)
            (cond ((null? exp) (make-empty-pool))
                  ((scheme-atomic? exp)
                   (extend-pool exp var-pool))
                  ((scheme-quoted? exp)
                   (extend-pool (scheme-quoted-var exp) var-pool))
                  ((scheme-composite? exp)
                   (extend-pool
                    exp
                    (map-pool findVarList
                              (scheme-composite->components exp)
                              var-pool)))
                  (else (error 'findVarList "Bad scheme exp ~s" exp))))))
      (findVarList exp (make-empty-pool)))))
