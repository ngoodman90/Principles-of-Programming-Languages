#lang racket

(require "equation-adt.rkt"
         "substitution-adt.rkt"
         "type-expression-adt.rkt"
         "type-expression-parser.rkt"
         "scheme-ast.rkt"
         "scheme-exp-to-pool.rkt"
         "scheme-exp-to-equations.rkt"
         "auxiliary.rkt")
(provide (all-defined-out))

;; Infer type of a Scheme expression:

;; Signature: infer-type(scheme-expr)
;; Purpose: Infer the type of a scheme expression using the equations method
;; Type: [Scheme-expression -> Type-expression]
;; Example: (te-concrete (infer-type (scheme-parse '(lambda (f x) (f (f x))))))
;;          ==> '((T_1 -> T_1) * T_1 -> T_1)
(define infer-type
  (lambda (scheme-exp)
    (let* ((pool (scheme-exp->pool scheme-exp))
           (equations (pool->equations pool))
           (sub (solve-equations equations))
           (te (val-of-index scheme-exp pool)))
      ;; get the type expression of the variable that represents scheme-exp,
      ;; in the substitution returned by solve-equations.
      (if (member te (sub->variables sub))
          (sub->expression-of-variable sub te)
          ;; not enough info to infer a type - return a variable
          te))))

;; type equation solving

;; Signature: solve-equations(equation-list)
;; Purpose: Solve the type equations and return the resulting substitution
;;          or error, if not solvable
;; Type: [List(Equation) -> Sub]
;; Example: (solve-equations
;;           (pool->equations
;;             (scheme-exp->pool
;;               (scheme-parse '((lambda (x) (x 11)) (lambda (y) y))))))
;;           ==> '((T_59 T_63 T_57 T_61 T_60 T_62 T_58)
;;           ==> '(sub (T_7 T_9 T_11 T_6
;;                      T_5 T_10 T_8)
;;                     (Number Number Number Number
;;                      (-> (* Number) Number)
;;                      (-> (* Number) Number)
;;                      (-> (* (-> (* Number) Number)) Number)))
(define solve-equations
  (lambda (equations)
    (solve equations (make-empty-sub))))


;; Signature: solve(equations, substitution)
;; Purpose: Solve the equations, starting from a given substitution.
;;          Returns the resulting substitution, or error, if not solvable
;; Type: [List(Equation)*Substitution -> Substitution]
;; Example: (solve (pool->equations
;;                  (scheme-exp->pool
;;                    (scheme-parse '((lambda (x) (x 11)) (lambda (y) y)))))
;;                 (make-empty-sub)) ==>
;;    '(sub (T_59 T_63 T_57 T_61 T_60 T_62 T_58)
;;          (Number Number Number Number
;;           (-> (* Number) Number)
;;           (-> (* Number) Number)
;;           (-> (* (-> (* Number) Number)) Number)))
(define solve
  (lambda (equations sub)
    (if (empty? equations)
        sub
        (let ((eq (make-equation
                   (sub-apply sub (equation->left (car equations)))
                   (sub-apply sub (equation->right (car equations))))))
          (letrec
              ((solve-var-eq  ; If one side of eq is a variable
                (lambda (var-part other-part)
                  (solve (cdr equations)
                         (sub-combine
                          sub
                          (make-sub (list var-part)
                                    (list other-part))))))
               (both-sides-atomic?
                (lambda (eq)
                  (and (atomic-te? (equation->left eq))
                       (atomic-te? (equation->right eq)))))
               (handle-both-sides-atomic
                (lambda (eq)
                  (if (equal-atomic-te? (equation->left eq)
                                        (equation->right eq))
                      (solve (cdr equations) sub)
                      (error
                       'solve
                       "equation contains unequal atomic types: ~e" eq)))))
            (cond
             ((type-variable? (equation->left eq))
              (solve-var-eq (equation->left eq) (equation->right eq)))
             ((type-variable? (equation->right eq))
              (solve-var-eq (equation->right eq) (equation->left eq)))
             ((both-sides-atomic? eq)
              (handle-both-sides-atomic eq))
             ((and (composite-te? (equation->left eq))
                   (composite-te? (equation->right eq))
                   (unifyable-structure eq))
              (solve (append (cdr equations) (split-equation eq)) sub))
             (else (error
                    'solve
                    "equation contains unknown type expression: ~s" eq))))))))


;; Signature: unifyable-structure(equation)
;; Purpose: Compars the structure of the type expressions of the equation
;; Type: Client view: [Equation -> Boolean]
;;       Implementation view: [LIST -> Boolean]
;; Example: (unifyable-structure
;;           '(equation (-> (* T_3) T_1) (-> (* T_2) T_1))) ==> #t
;;          (unifyable-structure
;;           '(equation T_0 (-> (* T_3) T_1))) ==> #f
(define unifyable-structure
  (lambda (eq)
   (let ((left (equation->left eq))
         (right (equation->right eq)))
     (or (and (proc-te? left) (proc-te? right)
              (= (tuple-te->length (proc-te->parameters left))
                 (tuple-te->length (proc-te->parameters right))))
         (and (tuple-te? left) (tuple-te? right)
              (= (tuple-te->length left) (tuple-te->length right)))))))


;; Signature: split-equation(equation)
;; Purpose: For an equation with unifyable type expressions,
;;          create equations for corresponding components.
;; Type: Client view: [Equation -> List(Equation)]
;;       Implementation view: [LIST -> LIST]
;; Example: (split-equation
;;           (make-equation '(-> (* T_3) (-> (* T_3) T_1))
;;                          '(-> (* T_3) (-> (* T_2) T_2)))) ==>
;;            ((equation (-> (* T_3) T_1) (-> (* T_2) T_2))
;;             (equation T_3 T_3))
;; Pre-condition: (and (composite? (equation->left eq))
;;                     (composite? (equation->right eq))
;;                     (unifyable-structure eq))
(define split-equation
  (lambda (eq)
    (letrec ((make-equations-from-components
              ;;create equations from corresponding type expressions
              (lambda (l1 l2) (map make-equation l1 l2))))
      (let ((left (equation->left eq))
            (right (equation->right eq)))
        (if (tuple-te? left)
            (make-equations-from-components
             (tuple-te->components left)
             (tuple-te->components right))
            (make-equations-from-components
             (cons (proc-te->return left)
                   (proc-te->parameter-components left))
             (cons (proc-te->return right)
                   (proc-te->parameter-components right))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
