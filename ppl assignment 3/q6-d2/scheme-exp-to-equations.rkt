#lang racket

(require "scheme-ast.rkt"
         "type-expression-adt.rkt"
         "type-expression-parser.rkt"
         "equation-adt.rkt"
         "scheme-exp-to-pool.rkt"
         "auxiliary.rkt"
         "utils.rkt")

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Constructor for equations for a Scheme expression:
;;; this constructor implements the second step of the type-inference-equations
;;; algorithm -- derive equations for all composite sub expressions of a
;;; given Scheme expression. Its input is a pool of pairs
;;; (Scheme-expression Type-var).
;;; A Scheme expression is mapped to a pool with scheme-exp->pool defined in
;;; "Scheme-exp-to-pool.rkt"

;; Signature: pool->equations(pool)
;; Purpose: Return a set of equations for a given Scheme expression
;; and a list of pairs: Sub-expression and its type variable
;; Type: [Pool -> LIST(Equation)]
;; Example: (pool->equations
;;            (scheme-exp->pool (scheme-parse '(lambda (x) (+ x 1)))))
;;          ==> '((T_0 (-> (* T_3) T_1))
;;                (T_2 (-> (* T_3 T_4) T_1)))
;; Pre-condition: pool is the result of (scheme-exp->pool exp) and
;;  'exp' is the abstract syntax of a legal Scheme expression.
(define pool->equations
  (lambda (pool)
    (let ((pool-without-vars
           ;; variable expressions generate no equations
           ;; but keep primitive operators
           (filter (lambda (exp-tvar)
                     (let ((e (pool->exp exp-tvar)))
                       (or (primitive-procedure? e)
                           (not (scheme-var? (pool->exp exp-tvar))))))
                   pool)))
      (map (lambda (exp) (make-equation-from-se exp pool))
           (map pool->exp pool-without-vars)))))


;; Signature: make-equation-from-se(se,pool)
;; Purpose: Return a single equation
;; Type: [Scheme-expression * Pool -> Equation]
;; Pre-condition: 'se' is a legal Scheme expression, and
;;                'se' is a member of 'pool'
(define make-equation-from-se
  (lambda (se pool)
    (letrec ((get-tvar-of-exp (lambda (exp) (val-of-index exp pool))))
      (cond
       ;; The type of procedure is (T1 * ... * Tn -> Te)
       ;; where Te is the type of the last exp in the body of the proc.
       ;; and   Ti is the type of each of the parameters.
       ((scheme-proc? se)
        (let ((left (get-tvar-of-exp se))
              (right (make-proc-te
                      (make-tuple-te
                       (map get-tvar-of-exp (scheme-proc->vars se)))
                      (get-tvar-of-exp (last (scheme-proc->body se))))))
          (make-equation left right)))
       ;; An application must respect the type of its operator
       ;; Type(Operator) = [T1 * .. * Tn -> Te]
       ;; Type(Application) = Te
       ((scheme-app? se)
        (let ((left (get-tvar-of-exp (scheme-app->operator se)))
              (right (make-proc-te
                      (make-tuple-te
                       (map get-tvar-of-exp (scheme-app->operands se)))
                      (get-tvar-of-exp se))))
          (make-equation left right)))
       ;; The type of a number is Number
       ((number? se)
        (let ((left (get-tvar-of-exp se))
              (right 'Number))
          (make-equation left right)))
       ;; The type of a boolean is Boolean
       ((boolean? se)
        (let ((left (get-tvar-of-exp se))
              (right 'Boolean))
          (make-equation left right)))
       ;; The type of a primitive procedure is given by the primitive.
       ((primitive-procedure? se)
        (let ((left (get-tvar-of-exp se))
              (right (get-primitive-type se)))
          (make-equation left right)))
       (else (error 'make-equation "Bad expression ~s" se))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Getters for a set of equations:

;; Signature: get-first-equation(equations)
;; Type: [LIST(LIST union Symbol) -> LIST union Symbol]
;; Example: (get-first-equation '((T_1 T_2) (T_2 T_3)))  ==> '(T_1 T_2)
(define get-first-equation car)


;; Signature: get-rest-equations(equations)
;; Type: [LIST(LIST union Symbol) -> LIST(LIST union Symbol)]
;; Example: (get-rest-equations '((T_1 T_2) (T_2 T_3)))  ==> '(T_2 T_3)
(define get-rest-equations cdr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Primitive-procedure-management:

;; The types of the recognized primitives are given
;; in the variable binary-primitive-types
(define binary-primitive-types
  (let ((binary-numeric-primitive-type (te-parse '(Number * Number -> Number)))
        (binary-logical-primitive-type (te-parse '(Number * Number -> Boolean))))
    (list (list '+ binary-numeric-primitive-type)
          (list '- binary-numeric-primitive-type)
          (list '> binary-logical-primitive-type))))

;; Signature: primitive-procedure?(se)
;; Purpose: Identify if a symbol denotes a primitive procedure
;;          for which a type is known.
;; Type: Client view: [Symbol -> Boolean]
;; Example: (primitive-procedure? '+)  ==> #t
(define primitive-procedure?
  (lambda (se)
    (let ((prim-se? (member se (map car binary-primitive-types))))
      (if prim-se? #t #f))))

;; Signature: get-primitive-type(se)
;; Type: Client view: [Symbol -> Procedure-TE]
;;       Supplier view: [Symbol -> LIST union Symbol]
;; Example: (get-primitive-type '+)  ==> '(-> (* 'Number 'Number) 'Number)]
;; Pre-condition: se is a symbol denoting a primitive-procedure
(define get-primitive-type
  (lambda (se)
    (let ((symbol-type-pair (assoc se binary-primitive-types)))
      (if symbol-type-pair
          (second symbol-type-pair)
          (error 'get-primitive-type "Not a known primitive ~s" se )))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
