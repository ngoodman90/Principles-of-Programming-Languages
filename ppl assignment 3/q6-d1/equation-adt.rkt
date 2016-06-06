#lang racket

(require "tagged.rkt"
         "scheme-ast.rkt"
         "type-expression-adt.rkt"
         "auxiliary.rkt"
         "utils.rkt")
(provide (all-defined-out))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Equation ADT
;; An equation is implemented as a tagged list of two type expressions.

;; Constructors:
;; Signature: make-equation(tel, ter)
;; Type:
;;  Client view: [Type-expression * Type-expression -> Equation]
;;  Supplier view: [LIST union Symbol * LIST union Symbol -> LIST]
;; Example:
;;  (make-equation
;;    '((-> (* Number Number) Number)  '(-> (* Number) Boolean))))
;;  ==> (equation (-> (* Number Number) Number)  (-> (* Number) Boolean))
(define make-equation
  (lambda (tel ter)
    (make-tagged 'equation (list tel ter))))

;; Signature: equation?(eq)
;; Type: [T -> Boolean]
;; Example: (equation? '((-> (* Number Number) Number)
;;                       (-> (* Number Number) Number))) ==> #t
(define equation? (tagged-by? 'equation))

;; Signature: equation->left(eq)
;; Type: [Equation -> TE]
;;  Example: (equation->left '( (-> (* Number Number) Number)  Boolean) )
;;         ==> (-> (* Number Number) Number)
(define equation->left
  (lambda (eq)
    (if (equation? eq)
        (car (tagged->content eq))
        (error 'equation->left "Bad equation ~s" eq ))))

;; Signature: equation->right(eq)
;; Type: [Equation -> TE]
;; Example: (equation->right '( (-> (* Number Number) Number)  Boolean) )
;;          ==> Boolean
(define equation->right
  (lambda (eq)
    (if (equation? eq)
        (cadr (tagged->content eq))
        (error 'equation->right "Bad equation ~s" eq ))))
