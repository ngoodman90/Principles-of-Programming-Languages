#lang racket

(require "../asp.rkt" "ds.rkt")
(provide (all-defined-out))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;  SUBSTITUTION-EVALUATOR  ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Type: [<Scheme-exp> -> Evaluator-value]
(define derive-eval
  (lambda (exp)
    (applicative-eval (derive exp))))

; Type: [<Scheme-exp> -> Scheme-type]
(define derive-eval-no-value-tag
  (lambda (exp)
    (let ((val (derive-eval exp)))
      (if (value? val)
          (value-content val)
          val))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main substitution-evaluation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Type: [(<Scheme-exp> union Evaluator-value) -> (Evaluator-value)]
;    Evaluator-value is a Scheme-type value, that is marked as a value. Procedures (user or primnitive)
;                    are distinguished from other values. Numbers and Booleans are also marked.
;    Note that the evaluator does not create closures of the underlying Scheme application.
; Pre-conditions: The given expression is legal according to the concrete syntax.
;                 No derived forms.
;                 Inner 'define' expressions are not legal.
; Post-condition: If the input is an already computed Evaluator-value, then output=input.
(define applicative-eval
  (lambda (exp)
    (cond ((atomic? exp) (eval-atomic exp))  ; Number or Boolean or Symbol or empty
          ((special-form? exp) (eval-special-form exp))
          ((evaluator-value? exp) exp)
          ((application? exp)
           (let ((renamed-exp (rename exp)))
             (apply-procedure (applicative-eval (operator renamed-exp))
                              (list-of-values (operands renamed-exp)))))
          (else (error 'eval "unknown expression type: ~s" exp)))))

; Type: [LIST -> LIST]
(define list-of-values
  (lambda (exps)
    (if (no-operands? exps)
        (list)
        (cons (applicative-eval (first-operand exps))
              (list-of-values (rest-operands exps))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Atomic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define eval-atomic
  (lambda (exp)
    (if (not (variable? exp))
        (make-value exp)
        (lookup-variable-value exp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Special form handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define eval-special-form
  (lambda (exp)
    (cond ((quoted? exp) (make-value exp))
          ((lambda? exp) (eval-lambda exp))
          ((definition? exp) (eval-definition exp))
          ((if? exp) (eval-if exp))
          ((defined?? exp) (eval-defined? exp))
          ((begin? exp) (eval-begin exp))
          )))

(define eval-defined?
  (lambda (exp)
    (let ((var (var-of-defined? exp))
          (sub the-global-environment))
    (letrec ((lookup
              (lambda (vars vals)
                (cond [(empty? vars) #f]
                      [(eq? var (car vars)) #t]
                      [else (lookup (cdr vars) (cdr vals))]))))
      (lookup (get-variables sub) (get-values sub))))
  ))

(define eval-lambda
  (lambda (exp)
    (make-procedure (lambda-parameters exp)
                    (lambda-body exp))))


(define eval-definition
  (lambda (exp)
    (add-binding! (make-binding (definition-variable exp)
                                (applicative-eval (definition-value exp))))
    'ok))

(define eval-if
  (lambda (exp)
    (if (true? (applicative-eval (if-predicate exp)))
        (applicative-eval (if-consequent exp))
        (applicative-eval (if-alternative exp)))))

(define eval-begin
  (lambda (exp)
    (eval-sequence (begin-actions exp))))

; (define eval-sequence
;   (lambda (exps)
;     (cond ((sequence-last-exp? exps) (applicative-eval (sequence-first-exp exps)))
;           (else (applicative-eval (sequence-first-exp exps))
;                 (eval-sequence (sequence-rest-exps exps))))))
; 

(define eval-sequence
  (lambda (exps)
    (let ((vals (map (lambda(e) (applicative-eval e))
                    exps)))
      (last vals))
    ))

(define true? 
  (lambda (x)
    (not (false? x))))

(define false? 
  (lambda (x)
     (or (eq? x #f) (equal? x '(value #f)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Application handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Type: [Evaluator-procedure*LIST -> Evaluator-value union Scheme-type]
(define apply-procedure
  (lambda (procedure arguments)
    (cond ((primitive-procedure? procedure)
           (apply-primitive-procedure procedure arguments))
          ((compound-procedure? procedure)
           (let ((parameters (procedure-parameters procedure))
                 (body (rename (procedure-body procedure))))
             (eval-sequence
              (substitute body parameters arguments))))
          (else (error 'apply "Unknown procedure type: ~s" procedure)))))


; Type: [Evaluator-primitive-procedure*LIST -> Evaluator-value]
; Retrieve the primitive implementation, retrieve content of the evaluator value
; arguments, apply and create a new evaluator value.
(define apply-primitive-procedure
  (lambda (proc args)
    (make-value
     (apply (primitive-implementation proc)
            (map (lambda (arg)
                   (cond ((evaluator-value? arg) (value-content arg))
                         ((primitive-procedure? arg) (primitive-implementation arg))
                         ((compound-procedure? arg) 
                           (error 'apply-primitive-procedure 
                                  "primitive-appliled-to-evaluator-procedure: ~s" arg))
                         (else arg)))
                 args)))
    ))


