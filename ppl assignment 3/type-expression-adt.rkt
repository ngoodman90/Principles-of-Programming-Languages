#lang racket

(require "tagged.rkt" "stack.rkt" "utils.rkt")
(provide (all-defined-out))

;;;;;;;;;;;  Implementation of the Type-Expression ADT ;;;;;;;;;;;;;;;;;;

;; Type Expression BNF grammar (without Void)
;; ------------------------------------------
;; TE -> Atomic-te | Composite-te | Type-variable
;; Atomic-te -> "Number" | "Boolean"
;; Composite-te -> Proc-te | Tuple-te
;; Non-Tuple-te -> Atomic-te | Proc-te | Type-variable
;; Proc-te -> "[" Tuple-te "->" Non-Tuple-te "]"
;; Tuple-te -> (Non-Tuple-te *)* Non-Tuple-te | "Empty"
;; Type-variable -> a symbol starting with an upper case

;; Abstract Syntax for TE
;; ----------------------
;; Tuple-te: Components: Components:LIST(Non-Tuple-te)
;; Proc-te:  Components: Parameters:Tuple-te, Return: Non-Tuple-te

;; ============================================================
;; Membership predicates
;; ---------------------

(define te?
  (lambda (e)
    (or (atomic-te? e)
        (composite-te? e)
        (type-variable? e))))

(define atomic-te?
  (lambda (e)
    (or (eq? e 'Number)
        (eq? e 'Boolean))))

(define composite-te?
  (lambda (e)
    (or (proc-te? e)
        (tuple-te? e))))

(define non-tuple-te?
  (lambda (e)
    (or (atomic-te? e)
        (proc-te? e)
        (type-variable? e))))


;; ============================================================
;; Atomic-te
;; ---------
;; Number, Boolean

;; ============================================================
;; Type-variable (not considered atomic)
;; Signature: type-variable?(te)
;; Type: [List union Symbol -> Boolean]
;; Example: (type-variable?
;;            (make-proc-te (make-tuple-te (list 'Number)) 'T1)) ==> #f
;;          (type-variable? 'T1) ==> #t
;;          (type-variable? '*) ==> #f
;;          (type-variable? 'Number) ==> #f
(define type-variable?
  (lambda (te)
    (and (not (atomic-te? te))
         (not (type-constructor? te))
         (not (tuple-te? te))
         (symbol? te))))

;; Signature: type-constructor?(sym)
;; Type: [T -> Boolean]
(define type-constructor?
  (lambda (e)
    (or (eq? e '*)
        (eq? e '->))))

;; Signature: equal-atomic-te?(te1 te2)
;; Type: [List union Symbol * List union Symbol -> Boolean]
;; Example: (equal-atomic-te? 'Number 'Number) ==> #t
(define equal-atomic-te?
  (lambda (te1 te2)
    (and (symbol? te1) (symbol? te2) (eq? te1 te2))))



;; ============================================================
;; Tuple-te
;; --------
;; Concrete: Tuple-te -> (Non-Tuple-te *)* Non-Tuple-te | "Empty"
;; Abstract: Tuple-te:
;;    Kinds: Empty-Tuple-te
;;           Non-Empty-Tuple-te: Components: Components:LIST(Non-Tuple-te)

;; Constructors
(define make-empty-tuple-te
  (lambda () 'empty-tuple-te))

;; Signature: make-tuple-te(te-list)
;; Type: Client view:[List(Non-Tuple-te)-> Tuple]
;;       Supplier view:[List -> List]
;; Example: (make-tuple-te (list 'Number
;;                               (make-proc-te
;;                                 (make-tuple-te (list 'Number)) 'T1)))
;;          ==> (* Number (-> (* Number) T1))
;;          (make-tuple-te '()) ==> 'empty-tuple-te
(define make-tuple-te
  (lambda (te-list)
    (if (empty? te-list)
        (make-empty-tuple-te)
        (make-tagged '* te-list))))

;; Membership predicate
(define empty-tuple-te?
  (lambda (e)
    (or (eq? e 'empty-tuple-te)
        (equal? e '(*)))))

(define tuple-te?
  (lambda (e)
    (or (empty-tuple-te? e)
        ((tagged-by? '*) e))))


;; Selectors
;; Signature: tuple-te->components(tte)
;; Type: Client view: [Tuple-te -> List(Non-Tuple-te)]
;;       Supplier view:[List -> List]
;; Example: (tuple-te->components (make-tuple-te (list 'Number 'Number)))
;;          ==> (Number Number)
(define tuple-te->components
  (lambda (tte)
    (if (empty-tuple-te? tte)
        (list)
        (if (tuple-te? tte)
            (tagged->content tte)
            (error 'tuple-te->components
                   "Bad application to Type expression value: ~s" tte )))))

;; Signature: tuple-te->length(tte)
;; Type: Client view: [Tuple-te -> Number]
;;       Supplier view:[List -> Number]
;; Example: (tuple-te->length (make-tuple-te (list 'Number 'Number))) ==> 2
(define tuple-te->length
  (lambda (tte)
    (if (tuple-te? tte)
        (length (tuple-te->components tte))
        (error 'tuple-te->components
               "Bad application to Type expression value: ~s" tte ))))


;; ============================================================
;; Proc-te
;; -------
;; Concrete: Proc-te -> "[" Tuple-te "->" Non-Tuple-te "]"
;; Abstract: Proc-te:  Components: Parameters:Tuple-te, Return: Non-Tuple-te

;; Constructor
;; Signature: make-proc-te(params, return)
;; Type: Client view: [TE * Non-Tuple-te -> Procedure]
;;       Supplier view: [(List union Symbol) * (List union Symbol) -> List]
;; Note: The constructor coerces a single param to a 1-element tuple
;; Example: (make-proc-te (make-tuple-te (list 'Number)) 'Number)
;;          ==> (-> (* Number) Number)
;;          (make-proc-te 'Number 'Number) ==> (-> (* Number) Number)
;;          (make-proc-te (make-empty-tuple-te) 'Number)
;;          ==> (-> empty-tuple-te Number)
(define make-proc-te
  (lambda (params return)
    (let ((tuple-params (if (tuple-te? params)
                            params
                            (make-tuple-te (list params)))))
      (make-tagged '-> (list tuple-params return)))))

;; Membership predicate
(define proc-te? (tagged-by? '->))

;; Selectors
;; Signature: proc-te->parameters(pte)
;; Type: [Procedure -> Tuple]
;; Example: (proc-te->parameters
;;            (make-proc-te (make-tuple-te (list 'Number)) 'T1))
;;          ==> (* Number)
(define proc-te->parameters
  (lambda (pte)
    (if (proc-te? pte)
        (first (tagged->content pte))
        (error 'proc-parameter-tuple-te
               "Bad application to Type expression value: ~s" pte))))

;; Signature: proc-te->parameter-components(pte)
;; Type: [Proc-te -> List(Non-Tuple-te)]
;; Example: (proc-parameter-tes
;;            (make-proc-te (make-tuple-te (list 'Number)) 'T1))
;;          ==> (Number)
(define proc-te->parameter-components
  (lambda (pte)
    (if (proc-te? pte)
        (tuple-te->components (proc-te->parameters pte))
        (error 'proc-te->parameter-components
               "Bad application to Type expression value: ~s" pte ))))

;; Signature: proc-te->return(pte)
;; Type: [Proc-te -> Non-Tuple-te]
;; Example: (proc-te->return
;;            (make-proc-te (make-tuple-te (list 'Number)) 'T1))
;;          ==> 'T1
(define proc-te->return
  (lambda (pte)
    (if (proc-te? pte)
        (second (tagged->content pte))
        (error 'proc-te->return
               "Bad application to Type expression value: ~s" pte ))))


;; ============================================================
;; Properties of TEs

;; Type: [TE -> List(Non-Tuple-te)]
;; Purpose: Traverse the abstract syntax tree
;; Example: (te->components (make-proc-te (make-tuple-te 'N) 'N))
;;          ==> (N N)
(define te->components
  (lambda (te)
    (cond ((atomic-te? te) (list))
          ((tuple-te? te) (tuple-te->components te))
          ((proc-te? te) (append (proc-te->parameter-components te)
                                 (list (proc-te->return te))))
          (else (error 'te->components
                       "Bad TE in te->components: ~s" te)))))

;; Signature: polymorphic-te?(te)
;; Type: Client view: [TE -> Boolean]
;; Purpose: A type expression is polymorphic if a type variable occurs
;;          in it at any level.
(define polymorphic-te?
  (lambda(te)
    (and (te? te)
         (or (type-variable? te)
             (and (composite-te? te)
                  (ormap polymorphic-te? (te->components te)))))))


;; Signature: high-order-te?(pte)
;; Type: Client view: [Proc-te -> Boolean]
;; Purpose: A procedure type expression is high order if a procedure
;;          occurs either as a parameter or as a return value.
(define high-order-te?
  (lambda(pte)
    (and (proc-te? pte)
         (or (ormap proc-te? (proc-te->parameter-components pte))
             (proc-te? (proc-te->return pte))))))

;; ============================================================
;; equivalent-tes: 2 TEs are equivalent up to variable renaming.

;; Signature: match-tvars-in-te$(te1 te2 succ fail)
;; Type:
;; Purpose:   Receives two type expressions (plus continuation procedures)
;;            and, incase they are equivalent, output a mapping between
;;            type variable they include. Otherwise, output FALSE.
;; Examples:
;; (match-tvars-in-te$ '(* T2 (-> (* Number T1) T1))
;;                     '(* T3 (-> (* Number T7) T5))
;;                     (lambda (x) x)
;;                     (lambda () #f)) ==> ((T2.T3) (T1.T7) (T1.T5))
;; (match-tvars-in-te$ '(-> (* Boolean T1) T1)
;;                     '(-> (* Number  T7) T5)
;;                     (lambda (x) x) (lambda () #f)) ==> #f
;; Tests:
;; (match-tvars-in-te$ '(-> (* Number T1) Number)
;;                     '(-> (* Number T7) T5)
;;                     (lambda (x) x) (lambda () #f)) ==> #f
;; (match-tvars-in-te$ '(*  (* Number T1) T1)
;;                     '(-> (* Number T7) T5)
;;                     (lambda (x) x) (lambda () #f)) ==> #f
;; (match-tvars-in-te$ '(-> (* T3) (-> (* T4 T2) Boolean))
;;                     '(-> (* T2) (-> (* T1 T3) Boolean))
;;                     (lambda (x) x)
;;                     (lambda () #f)) ==> '((T3 . T2) (T4 . T1) (T2 . T3))
(define match-tvars-in-te$
  (lambda (te1 te2 succ fail)
    (cond ((null? te1)
           (if (null? te2) (succ (list)) (fail)))
          ((type-variable? te1)
           (if (type-variable? te2)
               (succ (list (cons te1 te2)))
               (fail)))
          ((or (atomic-te? te1) (atomic-te? te2))
           (if (equal? te2 te1) (succ (list)) (fail)))
          ((or (empty-tuple-te? te1) (empty-tuple-te? te2))
           (if (equal? te2 te1) (succ (list)) (fail)))
          ((or (type-constructor? te1) (atomic-te? te2))
           (if (equal? te2 te1) (succ (list)) (fail)))
          (else (match-tvars-in-te$
                 (car te1) (car te2)
                 (lambda (sub-car)
                   (match-tvars-in-te$ (cdr te1) (cdr te2)
                                       (lambda (sub-cdr)
                                         (succ (append sub-car sub-cdr)))
                                       fail))
                 fail)))))

;; Signature: equivalent-tes?(te1 te2)
;; Type:      [TE * TE -> Boolean]
;; Purpose:   Check whether 2 type expressions are equivalent up to
;;            type variable renaming.
;; Examples:  (equivalent-tes? '(* T1 (-> (* Number T2) T3))
;;                             '(* T4 (-> (* Number T5) T6))) ==> #t
;; Tests:
;; (equivalent-tes? '(* T1 (-> (* Number T1) T3))
;;                  '(* T4 (-> (* Number T5) T6))) ==> #f
;; (equivalent-tes? '(* T1 (-> (* Number T2) T3))
;;                  '(* T4 (-> (* Number T4) T6))) ==> #f
;; (equivalent-tes? '(* T1 (-> (* Number  T2) T3))
;;                  '(* T4 (-> (* Boolean T5) T6))) ==> #f
(define equivalent-tes?
  (lambda (te1 te2)
    (let ((tvars-pairs (match-tvars-in-te$
                        te1 te2 (lambda (x) x) (lambda () #f))))
      (if tvars-pairs
          (let* ((tvars-unique-pairs (set->list (list->set tvars-pairs)))
                 (tvars-pairs-car    (map car tvars-unique-pairs))
                 (tvars-pairs-cdr    (map cdr tvars-unique-pairs)))
            (and (eq? (set-count (list->set tvars-pairs-car))
                      (length tvars-pairs-car))
                 (eq? (set-count (list->set tvars-pairs-cdr))
                      (length tvars-pairs-cdr))))
          #f))))
