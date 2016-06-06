#lang racket

(require "tagged.rkt"
         "type-expression-adt.rkt"
         "type-expression-parser.rkt" ;; for examples
         "auxiliary.rkt"
         "utils.rkt")

(provide (all-defined-out))


;;;;;;;;;;; Implementation of the Substitution ADT ;;;;;;;;;;;;;;;;;;
;; A substitution is represented as a 2 element list of equal length
;; lists of variables and type expression.
;; The empty substitution is the list (() ()).

;; Constructors:
;; Signature: make-sub(variables, tes)
;; Purpose: Create a substitution in which the i-th element of 'variables'
;;          is mapped to the i-th element of 'tes'.
;; Type: Client view: [LIST(Symbol)*LIST(Type-expression) -> Substitution]
;;       Supplier view: [LIST(Symbol)*LIST(LIST union Symbol)) -> LIST]
;; Example: (make-sub '(x y z)
;;             (map te-parse '(Number Boolean (Number -> Number))))
;;          ==> ((x y z) (Number Boolean (-> (* Number) Number)))
;;          (make-sub '(x y z)
;;             (map te-parse '(Number Boolean (z -> Number))))
;;          ==> error make-sub: circular substitution:
;;              ((x y z)
;;               (Number Boolean (-> (* z) Number)))
;;          (make-sub '(x y z)
;;             (map te-parse '(Number x (y -> Number))))
;;          ==> ((x y z) (Number x (-> (* y) Number)))
;; Pre-condition: (length variables) = (length tes)
;;                @@variables no repetitions? (set)
(define make-sub
  (lambda (variables tes)
    ;; if any variable substituting expression is circular
    (if (ormap occur? variables tes)
        (error 'make-sub "circular substitution: variables are ~s, tes are ~s"
               variables tes)
        (make-tagged 'sub (list variables tes)))))

(define make-empty-sub
  (lambda ()
    (make-sub '() '())))

;; Signature: occur?(var, te)
;; Purpose: check whether var  occurs in te
;; Type: [Symbol * TE -> Boolean]
;; Example:
;; (occur? 'T (te-parse '(Number -> Number))) ==> #f
;; (occur? 'T (te-parse '(T -> Number)))      ==> #t
(define occur?
  (lambda (var te)
    (cond ((atomic-te? te) #f)
          ((type-variable? te) (eq? var te))
          ((proc-te? te)
           (or (occur? var (proc-te->parameters te))
               (occur? var (proc-te->return te))))
          ((tuple-te? te)
           (ormap (lambda (te) (occur? var te))
                  (tuple-te->components te))))))


;; Selectors

;; Signature: sub->variables(sub)
;; Type: Client view: [Sub -> LIST(Var)]
;;       Supplier view: [LIST -> LIST(Symbol)]
;; Example: (sub->variables
;;            (make-sub '(x y z) (map te-parse '(T Boolean (T -> Number)))))
;;           ==> (x y z)
(define sub->variables
  (lambda (sub)
    (if (sub? sub)
        (car (tagged->content sub))
        (error 'sub->variables "sub is not a substitution: ~s" sub))))

;; Signature: sub->tes(sub)
;; Type: Client view: [Sub -> LIST(TE)]
;;       Supplier view: [LIST -> LIST(LIST union Symbol)]
;; Example: (get-tes
;;           (make-sub '(x y z) (map te-parse '(T Boolean (T -> Number)))))
;;          ==> (T Boolean (-> (* T) Number))
(define sub->tes
  (lambda (sub)
    (if (sub? sub)
        (cadr (tagged->content sub))
        (error 'sub->tes "sub is not a substitution: ~s" sub))))

;; Signature: sub->expression-of-variable(sub,var)
;; Type: Client view: [Sub*Var -> TE]
;;       Supplier view: [LIST*Symbol -> LIST union Symbol]
;; Example: (sub->expression-of-variable
;;            (make-sub '(x y z) '(Number Boolean T))
;;            'y) ==> 'Boolean
;; Pre-condition: sub is a non empty substitution that includes var.
(define sub->expression-of-variable
  (lambda (sub var)
    (letrec
        ((lookup
          (lambda (vars tes)
            (cond ((or (empty-sub? sub) (not (member var vars)))
                   (error 'sub->expression-of-variable
                     "var is not a variable of sub: sub is ~s. var is ~s"
                     sub var))
                  ((eq? var (car vars)) (car tes))
                  (else (lookup (cdr vars) (cdr tes)))))))
      (lookup (sub->variables sub) (sub->tes sub)))))


;; Signature: extend-sub(sub,var,te)
;; Type: Client view: [Sub * Var * TE -> Sub]
;;       Supplier view: [LIST * Symbol * LIST union Symbol -> LIST]
;; Examples:
;; (extend-sub
;;   (make-sub '(T1 T2 T3)
;;             (map te-parse '(S1 (S2 * Number) Boolean)))
;;   'S1
;;   (te-parse'(T21 * (Number * T23 -> T22)))) ==>
;; '((S1 T1 T2 T3)
;;   ((* T21 (-> (* Number T23) T22))
;;    (* T21 (-> (* Number T23) T22))     ;; Note substitution of S1
;;    (* S2 Number)
;;    Boolean))
;;
;; (extend-sub
;;   (make-sub '(T1 T2 T3)
;;             (map te-parse '(S1 (S2 * Number) Boolean)))
;;   'S1
;;   (te-parse '(S1 * (Number * T23 -> T22)))) ==>
;;  error make-sub: circular substitution:
;;  ((S1) ((* S1 (-> (* Number T23) T22))))
;;
;; (extend-sub
;;   (make-sub '(T1 T2 T3) (map te-parse '(S1 (S2 * Number) Boolean)))
;;   'S1
;;   (te-parse '(T1 * (Number * T23 -> T22)))) ==>
;;  error make-sub: circular substitution:
;;  ((S1 T1 T2 T3)
;;   ((* T1 (-> (* Number T23) T22))
;;    (* T1 (-> (* Number T23) T22)) ;; Note substitution of S1
;;    (* S2 Number)
;;    Boolean))
(define extend-sub
  (lambda (sub var te)
    (cond ((occur? var te)
           (error
            'extend-sub
            "Var occurs in type expression. Var is ~s, type expression is ~s"
            var te))
          ((empty-sub? sub) (make-sub (list var) (list te)))
          (else
           (let ((vars (sub->variables sub))
                 (tes (sub->tes sub))
                 (new-sub (make-sub (list var) (list te))))
             (if (member var vars)
                 (make-sub
                  vars
                  (map (lambda (sub-te)
                         (sub-apply new-sub sub-te))
                       tes))
                 (make-sub
                  (cons var vars)
                  (cons te (map (lambda (sub-te)
                                  (sub-apply new-sub sub-te))
                                tes)))))))))


;; ============================================================
;; Membership predicates

;; Signature: sub?(sub)
;; Type: [T -> Boolean]
;; Example:
;; (sub? (make-sub '(x y z) (map te-parse '(T1 T2 (T1 -> T2))))) => #t
;; (sub? (make-sub (list) (list))) ==> #t
;; (sub? '()) ==> #f
;; (sub? (make-sub '(x y z) (map te-parse '(T1 T2 (T1 -> z)))))
;;    error: make-sub: circular substitution: ((x y z) (T1 T2 (-> (* T1) z)))
(define sub?
  (lambda (tsub)
    (and ((tagged-by? 'sub) tsub)
         (let ((sub (tagged->content tsub)))
           (and (list? sub)
                (= (length sub) 2)
                (let ((vars (car sub))
                      (tes (cadr sub)))
                  (and (list? vars)
                       (andmap symbol? vars)
                       (list? tes)
                       (andmap te? tes)
                       (= (length vars) (length tes)))))))))


;; Signature: empty-sub?(sub)
;; Type: [T -> Boolean]
;; Example:
;; (empty-sub? (make-sub '(x y z) (map te-parser '(T1 T2 (T1 -> T2))))) ==> #f
;; (empty-sub? (make-sub (list) (list))) ==> #t
(define empty-sub?
  (lambda (sub)
    (and (sub? sub)
         (empty? (sub->variables sub))
         (empty? (sub->tes sub)))))

;; Signature: non-empty-sub?(sub)
;; Type: [T -> Boolean]
(define non-empty-sub?
  (lambda (sub) (not (empty? sub))))


;; ============================================================
;; Equality

;; Signature: sub-equal?(sub1,sub2)
;; Purpose: Check that 2 substitutions are equal - ignore order of vars.
;; Type: Client view: [Sub * Sub -> Boolean]
;;       Supplier view: [LIST * LIST -> Boolean]
;; Example:
;; (sub-equal? (make-sub '(x y z)
;;                       (map te-parse '(Number Boolean (Number -> Number))))
;;             (make-sub '(y x z)
;;                       (map te-parse '(Boolean Number (Number -> Number)))))
;; ==> #t
;; (sub-equal? (make-sub (list) (list)) (make-sub (list) (list))) ==> #t
(define sub-equal?
  (lambda (sub1 sub2)
    (and (sub? sub1)
         (sub? sub2)
         (set-equal? (sub->variables sub1) (sub->variables sub2))
         (andmap (lambda (var)
                   (equal? (sub->expression-of-variable sub1 var)
                           (sub->expression-of-variable sub2 var)))
                 (sub->variables sub1)))))

;; ============================================================
;; Signature: sub-apply(sub,te)
;; Type: [Sub * TE -> TE]
;; Example:
;; (sub-apply
;;   (make-sub '(T1 T2 T3)
;;             (map te-parse '(Number (T4 * Number) Boolean)))
;;   (te-parse '(T2 * (Number * T1 -> T1))))
;; ==>  (* (* T4 Number) (-> (* Number Number) Number))
;;
;; (sub-apply
;;   (make-sub (list) (list))
;;   (te-parse '(T2 * (Number * T1 -> T1))))
;; ==>  (* T2 (-> (* Number T1) T1))
(define sub-apply
  (lambda (sub te)
    (cond ((empty-sub? sub) te)
          ((atomic-te? te) te)
          (else
           (let ((vars (sub->variables sub))
                 (tes (sub->tes sub))
                 (sub-apply-this (lambda (te) (sub-apply sub te))))
             (cond ((type-variable? te)
                    (if (member te vars)
                        (sub->expression-of-variable sub te)
                        te))
                   ((tuple-te? te)
                    (make-tuple-te
                     (map sub-apply-this (tuple-te->components te))))
                   ((proc-te? te)
                    (make-proc-te
                     (make-tuple-te
                      (map sub-apply-this (proc-te->parameter-components te)))
                     (sub-apply-this (proc-te->return te))))
                   (else te)))))))


;; ============================================================
;; Signature: sub-combine(sub1,sub2)
;; Purpose: Returns the composition of substitutions s.t.:
;;  (sub-apply result te) === (sub-apply sub2 (sub-apply sub1 te))
;;
;; Type: [Sub * Sub -> Sub]
;; Example:
;; (sub-combine
;;   (make-sub '(T1 T2 T3)
;;             (map te-parse '(S1 (S2 * Number) Boolean)))
;;   (make-sub '(S1 S2)
;;             (map te-parse '((T21 * (Number * T11 -> T11)) T3)))) ==>
;; '((S2 S1 T1 T2 T3)
;;   (T3
;;    (* T21 (-> (* Number T11) T11))
;;    (* T21 (-> (* Number T11) T11))
;;    (* T3 Number)
;;    Boolean))

(define sub-combine
  (lambda (sub1 sub2)
    (cond ((empty-sub? sub1) sub2)
          ((empty-sub? sub2) sub1)
          (else (letrec ((combine
                          (lambda (sub vars tes)
                            (if (empty? vars)
                                sub
                                (combine
                                 (extend-sub sub (car vars) (car tes))
                                 (cdr vars) (cdr tes))))))
                  (combine sub1 (sub->variables sub2) (sub->tes sub2)))))))
