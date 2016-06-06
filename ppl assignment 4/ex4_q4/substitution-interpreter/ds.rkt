#lang racket

(require "../asp.rkt")
(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evaluator types:


(define evaluator-value?
  (lambda (val) (or (value? val)
                    (primitive-procedure? val) (compound-procedure? val))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Value:
; Type: [LIST -> LIST]
(define make-value
  (lambda (x) (attach-tag (list x) 'value)))

; Type: [T -> Boolean]
(define value?
  (lambda (s) (tagged-by? s 'value)))

; Type: [LIST -> LIST]
(define value-content
  (lambda (s) (car (get-content s))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Primitive procedure:
; Type: [T -> LIST]
(define make-primitive-procedure
  (lambda (proc)
    (attach-tag (list proc) 'primitive)))

; Type: [LIST -> T]
(define primitive-implementation
  (lambda (proc)
    (car (get-content proc))))

; Type: [T -> Boolean]
(define primitive-procedure?
  (lambda (proc)
    (tagged-by? proc 'primitive)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Closure:
; Type: [LIST(Symbol)*LIST -> LIST]
(define make-procedure
  (lambda (parameters body)
    (attach-tag (cons parameters body) 'procedure)))

; Type: [T -> Boolean]
(define compound-procedure?
  (lambda (p)
    (tagged-by? p 'procedure)))

; Type: [LIST -> LIST(Symbol)]
(define procedure-parameters
  (lambda (p)
    (car (get-content p))))

; Type: [LIST -> LIST]
(define procedure-body
  (lambda (p)
    (cdr (get-content p))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;SUBSTITUTUION-ADT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; Implementation of the Substitution for Scehme variables-values ADT ;;;;;;;;;;;;;;;;;;
;A substitution is represented as a 2 element list of equal length lists of variables and
;type expression. The empty substitution is the list (() ()).

; Constructors:

;Signature: make-sub(variables, values)
;Purpose: Create a substitution in which the i-th element of 'variables' is mapped to the
;         i-th element of 'values'.
;Type: Client view: [LIST(Symbol)*LIST(Scheme-expression union Evaluator-value) -> Sunstitution]
;      Supplier view: [LIST(Symbol)*LIST(LIST union Symbol)) -> lIST] 
;Example: (make-sub '(x y z) 
;              '(3 #t (lambda (x) (+ x 1))) ) ==> 
;                                   ((x y z) (3 #t (lambda (x) (+ x 1))))
;Prec-condition: (length variables) = (length values)
(define make-sub
  (lambda (variables values)
    (let ((sub (list variables values)))
      (if (sub? sub)
           sub
           (error 'make-sub: "Illegal substitution. Variables are ~s, values are ~s" variables values)))
  ))


; Getters:

;Signature: get-variables(sub)
;Type: Client view: [Sub -> LIST(Var)]
;      Supplier view: [LIST -> LIST(Symbol)] 
(define get-variables
  (lambda (sub) 
    (if (sub? sub)
        (car sub)
        (error 'Sub:get-variables "sub is not a substitution: ~s" sub))
    ))

;Signature: get-values(sub)
;Type: Client view: [Sub -> LIST(Scheme-expression union Evaluator-value)]
;      Supplier view: [LIST -> LIST(LIST union Symbol)] 
(define get-values
  (lambda (sub) 
    (if (sub? sub)
        (cadr sub)
        (error 'Sub:get-values "sub is not a substitution: ~s" sub))
    ))

;Signature: get-value-of-variable(sub,var,fail-cont)
;Type: Client view: [Sub*Var*[Empty->T] -> Scheme-expression union Evaluator-value]
;      Supplier view: [LIST*Symbol*[Empty->T] -> LIST union Symbol] 
;Purpose: If sub is an empty substitution or does not includes var, fail-cont is applied.
(define get-value-of-variable
  (lambda (sub var fail-cont)
    (letrec ((lookup
              (lambda (vars vals)
                (cond ((empty? vars)
                       (fail-cont))
                      ((eq? var (car vars)) (car vals))
                      (else (lookup (cdr vars) (cdr vals)))))))
      (lookup (get-variables sub) (get-values sub)))
  ))

;Signature: extend-sub(sub,var,value)
;Purpose: Adds var-value binding, if var is not already in sub. Otehrwise -- error
;Type: Client view: [Substitution*Var*(Scheme-expression union Evaluator-value) -> Substitution]
;      Supplier view: [LIST*Symbol*LIST union Symbol -> LIST]
(define extend-sub
  (lambda (sub var value)
        (let ((vars (get-variables sub))
              (values (get-values sub)))
            (if (member var vars)
                (error 'extend-sub "variable repetition: ~s ~s)" sub var)
                (make-sub
                  (cons var vars)
                  (cons value values)))) ;)
  ))


;;;;;;;;;;;;;;;
;identifiers:

;Signature: sub?(sub)
;Type: [T -> Boolean] 
;Example: (sub? (make-sub '(x y z) 
;                         '(3 #t (lambda (x)(+ x 1))) ) ==> #t
;         (sub? (make-sub (list) (list))) ==> #t
;         (sub? '()) ==> #f
(define sub?
  (lambda (sub) 
    (and (list? sub)
         (= (length sub) 2) 
         (let ((vars (car sub))
               (values (cadr sub)))
           (and (list? vars)
                (empty? (filter (lambda (sym) (not (symbol? sym))) vars))
                (list? values)
                (empty? (filter 
                         (lambda (val) (not (or (scheme-expr? val) (evaluator-value? val))))
                         values))
                (= (length vars) (length values)))))
    ))

    
;Signature: empty-sub?(sub)
;Type: [T -> Boolean] 
;Example: (empty-sub? (make-sub '(x y z) 
;                       (list 'Number 'Boolean 
;                         '(3 #t (lambda (x)(+ x 1))) ) ==> #f
;         (empty-sub? (make-sub (list) (list))) ==> #t
;Pre-condition: (sub? sub)
(define empty-sub?
  (lambda (sub)
    (and (sub? sub)
         (empty? (get-variables sub))
         (empty? (get-values sub)))
    ))
 
;Signature: non-empty-sub?(sub)
;Type: [T -> Boolean] 
;Example: (non-empty-sub? (make-sub '(x y z) 
;                         '(3 #t (lambda (x)(+ x 1))) ) ==> #t
;         (non-empty-sub? (make-sub (list) (list))) ==> #f
;Pre-condition: (sub? sub)
(define non-empty-sub?
  (lambda (sub) (not (empty? sub))))


;;;;;;;;;;;;;;;;;
;equality:

;Signature: sub-equal?(sub1,sub2)
;Type: Client view: [Sub*Sub -> Boolean]
;      Supplier view: [LIST*LIST -> Boolean] 
;Example: (sub-equal? (make-sub '(x y z) 
;                               '(3 #t (lambda (x)(+ x 1))) )
;                     (make-sub '(y x z) 
;                               '(#t 3 (lambda (x)(+ x 1))) ) ) ==> #t
;         (sub-equal? (make-sub (list) (list)) (make-sub (list) (list)) ) ==> #t
(define sub-equal?
  (lambda (sub1 sub2)
    (and (sub? sub1)
         (sub? sub2)
         (set-equal? (get-variables sub1) (get-variables sub2))
         (empty? (filter false?
                         (map (lambda (var)
                                   (equal? (get-value-of-variable sub1 var (lambda () #f))
                                           (get-value-of-variable sub2 var (lambda () #f))))
                                 (get-variables sub1)))))
    ))



;;;;;;;;;;;;;;;;;
;Signature: substitute(sub,expr)
; Purpose: Consistent replacement of all FREE occurrences of the substitution variables in 
;          'expr' by the values of the substitution, respectively.
;          'expr' is a Scheme expression.
;Type: Client view: [Substitution*Scheme-expression union Evaluator-value -> Scheme-expression union Evaluator-value]
;      Supplier view: [LIST*(LIST union Symbol) -> LIST union Symbol]
;Example: (substitute 
;            (make-sub '(x y z) '(3 4 (lambda (x1)(+ x1 1))))
;            '(lambda (x2)(+ x2 y))) ) ==>  (lambda (x2)(+ x2 4))
; Pre-conditions: (1) substitution is not performed on 'define' or 'let' expressions
;                     or on expressions containing such sub-expressions.
;                 (2) 'expr' is already renamed. Therefore, the substitution variables have
;                     no bound occurrences in expr.
(define substitute
  (lambda (expr variables values)
    (if (evaluator-value? expr)
        (substitution-application-to-value (make-sub variables values) expr)
        (substitution-application-to-expr (make-sub variables values) expr))))


(define substitution-application-to-expr
  (lambda (sub expr)
    (cond ((empty-sub? sub) expr)
          ((or (number? expr) (boolean? expr) (quoted? expr)) expr)
          (else (let ((vars (get-variables sub))
                      (values (get-values sub)))
                  (cond ((variable? expr)
                         (if (member expr vars)
                             (get-value-of-variable sub expr (lambda () expr))
                             expr))
                        (else ; expression is a list of expressions, lambda, application, cond.
                         (map (lambda (e) 
                                (substitute e vars values))
                              expr))))))))

  
(define substitution-application-to-value
  (lambda (sub val-expr)
    (let ((vars (get-variables sub))
          (values (get-values sub)))
      (cond ((primitive-procedure? val-expr) val-expr)
            ((value? val-expr)
             (if (atomic? (value-content val-expr))
                 val-expr
                 (make-value (map (lambda (e) (substitute e vars values))
                                  (value-content val-expr)))))
            ((compound-procedure? val-expr)
             (make-procedure (procedure-parameters val-expr)
                             (map (lambda (e) (substitute e vars values))
                                  (procedure-body val-expr))))))
  ))  

  
                      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;       RENAMING PROCEDURE       ;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Signature: rename(exp)
; Purpose: Consistently rename bound variables in 'exp'.
; Type: [<Scheme-exp> -> <Scheme-exp>]
(define rename
  (letrec ((make-new-names
            (lambda (old-names)
              (if (null? old-names)
                  (list)
                  (cons (gensym) (make-new-names (cdr old-names))))))
           (replace
            (lambda (val-exp)
              (cond ((primitive-procedure? val-exp) val-exp)
                    ((value? val-exp)
                     (if (atomic? (value-content val-exp))
                         val-exp
                         (make-value (map rename (value-content val-exp)))))
                    ((compound-procedure? val-exp)
                     (let* ((params (procedure-parameters val-exp))
                            (new-params (make-new-names params))
                            (renamed-subs-body (map rename (procedure-body val-exp)))
                            (renamed-body (substitute renamed-subs-body params new-params)))
                       (make-procedure new-params renamed-body)))))))
    (lambda (exp)
      (cond ((atomic? exp) exp)
            ((lambda? exp)
             (let* ((params (lambda-parameters exp))
                    (new-params (make-new-names params))
                    (renamed-subs (map rename exp)))
               (substitute renamed-subs params new-params)))  ; replaces free occurrences
            ((evaluator-value? exp) (replace exp))
            (else (map rename exp))))))

                               
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;  Global environment ADT implementation  ;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;; Construction ;;;;;;;;;;;;;
; Type: Client view: [Void -> GE]
;       Supplier view: [Void -> LIST]
; The global environment is implemented as a substitution: A 2-element list of symbols and their
; values
(define make-the-global-environment
  (lambda ()
    (let* ((primitive-procedures
            (list (list 'car car)
                  (list 'cdr cdr)
                  (list 'cons cons)
                  (list 'null? null?)
                  (list '+ +)
                  (list '* *)
                  (list '/ /)
                  (list '> >)
                  (list '< <)
                  (list '- -)
                  (list '= =)
                  (list 'list list)
                  (list 'append append)
                    ;;      more primitives
                  ))
           (prim-variables (map car primitive-procedures))
           (prim-values (map (lambda (x) (make-primitive-procedure (cadr x)))
                             primitive-procedures)) )
      (make-sub prim-variables prim-values))
  ))


;;;;;;;; INTERPRETER SETUP: ENVIRONMENT VARIABLE the-global-environment ;;;;;;;;;;;;;;

(define the-global-environment (make-the-global-environment))


;;;;;;;;;;; Selection:

; Type: [Symbol -> T]
(define lookup-variable-value
  (lambda (var)
    (get-value-of-variable the-global-environment 
                           var
                           (lambda () (error 'lookup-variable-value "undefined variable ~s" var)))
    ))




;;;;;;;;;;;;; Mutation:

; Type: [PAIR(Symbol,T) -> Void]
(define add-binding!
  (lambda (binding)
    (let ((bvar (binding-variable binding))
          (bval (binding-value binding)))
      (set! the-global-environment (extend-sub the-global-environment bvar bval)))
  ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bindings

; Type: [Symbol*T -> PAIR)Symbol,T)]
(define make-binding
  (lambda (var val)
    (cons var val)))

; Type: [PAIR(Symbol,T) -> Symbol]
(define binding-variable
  (lambda (binding)
    (car binding)))

; Type: [PAIR(Symbol,T) -> T]
(define binding-value
  (lambda (binding)
    (cdr binding)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
