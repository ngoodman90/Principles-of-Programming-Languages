#lang racket

(require "../asp.rkt" "ge-adt.rkt")
(provide (all-defined-out))


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
    (list variables values)
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

;Signature: get-value-of-variable(sub,var)
;Type: Client view: [Sub*Var -> Scheme-expression union Evaluator-value]
;      Supplier view: [LIST*Symbol -> LIST union Symbol] 
;Pre-condition: sub is a non empty substitution that includes var.
(define get-value-of-variable
  (lambda (sub var)
    (letrec ((lookup
              (lambda (vars vals)
                (cond ((or (empty-sub? sub) (not (member var vars)))
                    (error 'get-value-of-variable 
                     "sub is an empty substitution or var is not a variable of sub: ~s ~s" sub var))
                      ((eq? var (car vars)) (car vals))
                      (else (lookup (cdr vars) (cdr vals)))))))
      (lookup (get-variables sub) (get-values sub)))
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
                                   (equal? (get-value-of-variable sub1 var)
                                           (get-value-of-variable sub2 var)))
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
        (substitution-application-to-expr (make-sub variables values) expr))
  ))


(define substitution-application-to-expr
  (lambda (sub expr)
    (cond ((empty-sub? sub) expr)
          ((or (number? expr) (boolean? expr) (quoted? expr)) expr)
          (else (let ((vars (get-variables sub))
                      (values (get-values sub)))
                  (cond ((variable? expr)
                         (if (member expr vars)
                             (get-value-of-variable sub expr)
                             expr))
                        (else ; expression is a list of expressions, lambda, application, cond.
                         (map (lambda (e) 
                                (substitute e vars values))
                              expr))))))
    ))

  
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

  
;Signature: extend-sub(sub,var,value)
;Purpose: Adds var-value binding, if var is not already in sub. Otehrwise -- error
;Type: Client view: [Substitution*Var*(Scheme-expression union Evaluator-value) -> Substitution]
;      Supplier view: [LIST*Symbol*LIST union Symbol -> LIST]
(define extend-sub
  (lambda (sub var value)
;    (if (empty-sub? sub)
;        (make-sub (list var) (list value))
        (let ((vars (get-variables sub))
              (values (get-values sub)))
            (if (member var vars)
                (error 'extend-sub "variable repetition: ~s ~s)" sub var)
                ;sub
                (make-sub
                  (cons var vars)
                  (cons value values)))) ;)
  ))


;Signature: substitution-combination(sub1,sub2)
;Type: Client view: [Substitution*Substitution -> Substitution]
;      Supplier view: [LIST*LIST -> LIST]
;Example: (substitution-combination 
;           (make-sub '(T1 T2 T3) (list 'S1 (make-tuple-te (list 'S2 'Number)) 'Boolean))
;           (make-sub '(S1 S2)
;            (list (make-tuple-te (list 'T21
;                                       (make-proc-te (make-tuple-te (list 'Number 'T1)) 'T11)))
;                  'T3))) ==>
;                     ((S2 S1 T1 T2 T3) 
;                      (T3 (* T21 (-> (* Number T11) T11)) 
;                      (* T21 (-> (* Number T11) T11)) (* T3 Number) Boolean))
(define substitution-combination
  (lambda (sub1 sub2)
    (cond ((empty-sub? sub1) sub2)
          ((empty-sub? sub2) sub1)
          (else (let ((vars2 (get-variables sub2))
                      (tes2 (get-values sub2)))
                  (substitution-combination
                     (extend-sub sub1 (car vars2) (car tes2))
                     (make-sub (cdr vars2) (cdr tes2))))))
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

                               
    
