 #lang racket

(require "../asp.rkt")
(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;  Data structures  ;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evaluator Procedure types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
; Type: [LIST(Symbol)*LIST*Env -> LIST]
(define make-procedure
  (lambda (parameters body env)
    (attach-tag (list parameters body env) 'procedure)))

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
    (cadr (get-content p))))

; Type: [LIST -> Env]
(define procedure-environment
  (lambda (p)
    (caddr (get-content p))))


  


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
;Type: Client view: [Sub*Var -> Scheme-expression union Evaluator-value]
;      Supplier view: [LIST*Symbol -> LIST union Symbol] 
;Pre-condition: sub is a non empty substitution that includes var. 
;               If not: The fail continuation is applied.
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




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evaluator environment types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Frames
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Frame constructor: A frame is a mapping from Scheme variables to Scheme values.
; Type: Client view: [[LIST(Symbol)*LIST -> Frame]
; Implementation: A substitution from Scheme variables to Scheme values. 
; Precondition: All preconditions for corerct Frame implementation are already 
;               checked in the Sunstitution constructor.
(define make-frame 
  (lambda (variables values)
    (make-sub variables values)
    ))

; Alternative procedural implementation for frames:
;(define make-frame 
;  (lambda (variables values)
;    (letrec ((lookout 
;              (lambda (vars vals)
;                (lambda (var)
;                  (cond ((empty? vars) empty)
;                        ((eq? var (car vars)) (make-binding (car vars) (car vals)))
;                        (else ((lookout (cdr vars) (cdr vals))
;                                var)))))))
;      (lookout variables values))
;  ))


; Frame constructor: 
; Purpose: Produces a new frame that extends the given frame with the new binding.
; Type: Client view: [Binding*Frame -> Frame]
(define extend-frame 
  (lambda (binding frame)
    (let ((bvar (binding-variable binding))
          (bval (binding-value binding)))
      (extend-sub frame bvar bval))
 ))


;Frame getter:
; Purpose: Get the value of a given variable. If undefined, apply the given fail continuation
; Type: Client view:  [Frame*Variable*Procedure -> T]
(define frame->lookup-variable-value get-value-of-variable)   
          ;The substitution getter (lambda (sub var fail-cont) ...}


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Environment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Environment constructor: ADT type is [Frame*Env -> Env]
; An environment is implemented as a list of boxed frames. The box is needed because the 
; last frame, i.e., the global environment, is changed following a variable definition. 
; Type: [[Symbol -> PAIR(Symbol,T) union {empty}]*
;       LIST(Box([Symbol -> PAIR(Symbol,T) union {empty}])) ->
;                                        LIST(Box([Symbol -> PAIR(Symbol,T) union {empty}]))]
(define extend-env 
  (lambda (frame base-env)
    (cons (box frame) base-env)))

; Environment selectors
; Input type is an environment, i.e., LIST(Box([Symbol -> PAIR(Symbol,T) union {empty}]))
(define enclosing-env (lambda (env) (cdr env)))
(define first-boxed-frame (lambda(env) (car env)))
(define first-frame (lambda(env) (unbox (first-boxed-frame env))))


; Environment selector: ADT type is [Var*Env -> T]
; Purpose: If the environment is defined on the given variable, selects its value
; Type: Client side: [Env*Symbol -> T]
;     Supplier side: [LIST(Box(Frame))*Symbol -> T]
;Implementation: Uses the frame->lookup-variable-value getter of frames.
(define lookup-variable-value 
  (lambda (env var)
    (letrec ((defined-in-env        ; ADT type is [Var*Env -> Binding union {empty}]
               (lambda (var env)
                 (if (empty-env? env)
                     (error 'lookup "variable not found: ~s\n  env = ~s" var env)
                     (frame->lookup-variable-value 
                       (first-frame env) 
                       var
                       (lambda () (defined-in-env var (enclosing-env env))))))
               ))
      (defined-in-env var env))
    ))

; Environment identification predicate
; Type: [T -> Boolean]
(define empty-env? 
  (lambda (env)
    (eq? env the-empty-environment)))

; Global environment mutator: ADT type is [Binding -> Unit]
; Type: [PAIR(Symbol,T) -> Unit]
; Note: Mutation is enabled only for the global environment
(define add-binding! 
  (lambda (binding)
    (let ((frame (first-frame the-global-environment)))
      (set-box! (first-boxed-frame the-global-environment)
                (extend-frame binding frame)))
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;  Global environment construction  ;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define the-empty-environment '())

; Type Client view: [Void -> GE]
;      Supplier view: [Void -> LIST(Box(LIST)) union {empty}
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
                   (list 'box box)
                   (list 'unbox unbox)
                   (list 'set-box! set-box!)
                   ;; more primitives
                   ))
           (prim-variables (map car primitive-procedures))
           (prim-values (map (lambda (x) (make-primitive-procedure (cadr x)))
                               primitive-procedures))
           (frame (make-frame prim-variables prim-values)))
    (extend-env frame the-empty-environment))))

(define the-global-environment (make-the-global-environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
