#lang racket
(require "tagged.rkt" "utils.rkt")
(provide (all-defined-out))

;; Abstract syntax for Scheme subset
;; BNF
;; <scheme-exp> :: <scheme-atomic> | <scheme-composite> | <scheme-define>
;; <scheme-define> :: (define <scheme-var> <scheme-exp>)
;; <scheme-atomic> :: <scheme-var> | <number> | <bool>
;; <scheme-composite> :: <scheme-proc> | <scheme-app> | <scheme-let> | <scheme-let*> | <scheme-letrec> | <scheme-if>
;; <scheme-proc> :: (lambda (<scheme-var>*) <scheme-exp>+)
;; <scheme-app> :: (<scheme-exp>+)
;; <scheme-let> :: (let (<scheme-binding>*) <scheme-exp>+)
;; <scheme-let*> :: (let* (<scheme-binding>*) <scheme-exp>+)
;; <scheme-letrec> :: (letrec (<scheme-binding>*) <scheme-exp>+)
;; <scheme-if> :: (if <scheme-exp> <scheme-exp> <scheme-exp>)
;; <scheme-binding> :: (<scheme-var> <scheme-exp>)

;; Abstract syntax types:
;; Scheme-define: Components var:Scheme-var val:Scheme-exp
;; Scheme-proc: Components vars:List(Scheme-var)
;;                         body:List(Scheme-exp)
;; Scheme-app:  Components operator:Scheme-exp
;;                         operands:List(Scheme-exp)
;; Scheme-let: Components: bindings:List(Pair(Scheme-var, Scheme-exp))
;;                         body:List(Scheme-exp)
;; Scheme-let*: Components: bindings:List(Pair(Scheme-var, Scheme-exp))
;;                          body:List(Scheme-exp)
;; Scheme-letrec: Components: bindings:List(Pair(Scheme-var, Scheme-exp))
;;                          body:List(Scheme-exp)
;; Scheme-if: Components: test:Scheme-exp, then:Scheme-exp, else:Scheme-exp

;; ============================================================
;; union types (have no constructors and no selectors)

; Signature: scheme-exp?(exp)
; Type: [T -> Boolean]
; Purpose: Type predicate for scheme-exp
; Pre-conditions: -
; Tests: 
; (scheme-exp? (make-scheme-var 'a)) --> #t
; (scheme-exp? 2) --> #t
; (scheme-exp? #f) --> #t
; (scheme-exp? '(a)) --> #f
(define scheme-exp?
  (lambda (exp)
    (or (scheme-atomic? exp)
        (scheme-composite? exp)
        (scheme-define? exp))))

; Signature: scheme-atomic?(exp)
; Type: [T -> Boolean]
; Purpose: Type predicate for scheme-atomic
; Pre-conditions: -
; Tests:
; (scheme-atomic? (make-scheme-var 'a)) --> #t
; (scheme-atomic? 2) --> #t
; (scheme-atomic? #f) --> #t
; (scheme-atomic? 'a) --> #f
; (scheme-atomic '(a)) --> #f
(define scheme-atomic?
  (lambda (exp)
    (or (scheme-var? exp)
        (number? exp)
        (boolean? exp))))

; Signature: scheme-composite?(exp)
; Type: [T -> Boolean]
; Purpose: Type predicate for scheme-composite
; Pre-conditions: -
; Tests: 
; (scheme-composite? 2) --> #f
; (scheme-composite? #f) --> #f
; (scheme-composite? (make-scheme-var 'a)) --> #f
; (scheme-composite? (make-scheme-app '+ 2 3)) --> #t
; (scheme-composite? (make-scheme-proc (list (make-scheme-var 'a)) (list (make-scheme-app '+ (make-scheme-var 'a) 2)))) --> #t
(define scheme-composite?
  (lambda (exp)
    (or (scheme-proc? exp)
        (scheme-app? exp)
        (scheme-let? exp)
        (scheme-let*? exp)
        (scheme-letrec? exp)
        (scheme-if? exp))))


;; ============================================================
;; Atomic types

; Signature: scheme-var?(x)
; Type: [T -> Boolean]
; Purpose: Type predicate for scheme-var
; Pre-conditions: -
; Tests:
; (scheme-var? (make-scheme-var 'a)) --> #t
; (scheme-var? 2) --> #f
(define scheme-var? (lambda (x) (symbol? x)))

; Signature: make-scheme-var(s)
; Type: [Symbol -> Scheme-var]
; Purpose: Value constructor for scheme-var
; Pre-conditions: -
; Tests:
; (make-scheme-var 'a) --> 'a
(define make-scheme-var (lambda (s) s))


;; ============================================================
;; compound types

; ------------------------------
; SCHEME-DEFINE
; Signature: make-scheme-define(var, val)
; Type: [Scheme-var * Scheme-exp -> Scheme-define]
; Purpose: Value constructor for Scheme-define
; Pre-conditions: -
; Tests:
; (make-scheme-define
;    (make-scheme-var 'a)
;    (make-scheme-app '+ (list 3 2)))
; --> '(define a (app + (3 2)))
(define make-scheme-define
  (lambda (var val)
    (make-tagged 'define (list var val))))

(define scheme-define? (tagged-by? 'define))

; Signature: scheme-define->var(exp)
; Type: [Scheme-define -> Scheme-var]
; Purpose: Accessor of component var in scheme-define
; Pre-conditions: -
; Tests:
; (scheme-define->var
;  (make-scheme-define
;     (make-scheme-var 'a)
;     (make-scheme-app '+ (list 3 2))))
; --> 'a
(define scheme-define->var
  (lambda (exp)
    (first (tagged->content exp))))

; Signature: scheme-define->val(exp)
; Type: [Scheme-define -> Scheme-exp]
; Purpose: Accessor of component val in scheme-define
; Pre-conditions: -
; Tests:
; (scheme-define->val
;   (make-scheme-define
;     (make-scheme-var 'a)
;     (make-scheme-app '+ (list 3 2))))
; --> '(app + (3 2))
(define scheme-define->val
  (lambda (exp)
    (second (tagged->content exp))))


; ------------------------------
; SCHEME-PROC
; Signature: make-scheme-proc(vars, body)
; Type: [LIST(Scheme-var) * LIST(Scheme-exp) -> Scheme-proc]
; Purpose: Value constructor for Scheme-proc
; Pre-conditions: No repetitions in vars
; Tests:
; (make-scheme-proc
;    (list (make-scheme-var 'a))
;    (list (make-scheme-app '+ (list (make-scheme-var 'a) 2))))
; --> '(lambda (a) ((app + (a 2))))
(define make-scheme-proc
  (lambda (vars body)
    (make-tagged 'lambda (list vars body))))

(define scheme-proc? (tagged-by? 'lambda))

; Signature: scheme-proc->vars(exp)
; Type: [Scheme-proc -> LIST(Symbol)]
; Purpose: Accessor of component vars in scheme-proc
; Pre-conditions: -
; Tests:
; (scheme-proc->vars 
;  (make-scheme-proc
;     (list (make-scheme-var 'a))
;     (list (make-scheme-app '+ (list (make-scheme-var 'a) 2)))))
; --> '(a)
(define scheme-proc->vars
  (lambda (exp)
    (first (tagged->content exp))))

; Signature: scheme-proc->body(exp)
; Type: [Scheme-proc -> LIST(Scheme-exp)]
; Purpose: Accessor of component body in scheme-proc
; Pre-conditions: -
; Tests:
; (scheme-proc->body
;   (make-scheme-proc
;     (list (make-scheme-var 'a))
;     (list (make-scheme-app '+ (list (make-scheme-var 'a) 2)))))
; --> '((app + (a 2)))
(define scheme-proc->body
  (lambda (exp)
    (second (tagged->content exp))))

;; ------------------------------
;; SCHEME-APP

; Signature: make-scheme-app(operator, operands)
; Type: [Scheme-exp * LIST(Scheme-exp) -> Scheme-app]
; Purpose: Value constructor for Scheme-app
; Pre-conditions: -
; Tests: 
; (make-scheme-app '+ (list (make-scheme-var 'a) 2))
; --> '(app + (a 2))
(define make-scheme-app
  (lambda (operator operands)
    (make-tagged 'app (list operator operands))))

(define scheme-app? (tagged-by? 'app))

; Signature: scheme-app->operator(app)
; Type: [Scheme-app -> Scheme-exp]
; Purpose: Accessor for component operator in scheme-app
; Pre-conditions: -
; Tests:
; (scheme-app->operator (make-scheme-app '+ (list (make-scheme-var 'a) 2)))
; --> '+
(define scheme-app->operator
  (lambda (app)
    (first (tagged->content app))))

; Signature: scheme-app->operands(app)
; Type: [Scheme-app -> LIST(Scheme-exp)]
; Purpose: Accessor for component operands in scheme-app
; Pre-conditions: -
; Tests:
; (scheme-app->operands (make-scheme-app '+ (list (make-scheme-var 'a) 2)))
; --> '(a 2)
(define scheme-app->operands
  (lambda (app)
    (second (tagged->content app))))

;; ------------------------------
;; SCHEME-LET

; Signature: make-scheme-let(bindings, body)
; Type: [LIST(Pair(Scheme-var, Scheme-exp)) * LIST(Scheme-exp) -> Scheme-let]
; Purpose: Value constructor for scheme-let
; Pre-conditions: -
; Tests: 
; (make-scheme-let (list (cons (make-scheme-var 'a) 2)) 
;                        (list (make-scheme-app '+ (list (make-scheme-var 'a) 3))))
; --> '(let ((a . 2)) ((app + (a 3))))
(define make-scheme-let
  (lambda (bindings body)
    (make-tagged 'let (list bindings body))))

(define scheme-let? (tagged-by? 'let))

; Signature: scheme-let->bindings(let)
; Type: [Scheme-let -> LIST(Pair(Scheme-var, Scheme-exp))]
; Purpose: Accessor 
; Pre-conditions: -
; Tests:
; (scheme-let->bindings (make-scheme-let (list (cons (make-scheme-var 'a) 2)) 
;                          (list (make-scheme-app '+ (list (make-scheme-var 'a) 3)))))
; --> '((a . 2))
(define scheme-let->bindings
  (lambda (let)
    (first (tagged->content let))))

; Signature: scheme-let->vars(let)
; Type: [Scheme-let -> LIST(Scheme-var)]
; Purpose: Extract the list of variables that are bound in the bindings of the let
; Pre-conditions: -
; Tests:
; (scheme-let->vars (make-scheme-let (list (cons (make-scheme-var 'a) 2)) 
;                           (list (make-scheme-app '+ (list (make-scheme-var 'a) 3)))))
; --> '(a)
(define scheme-let->vars
  (lambda (let)
    (map car (scheme-let->bindings let))))

; Signature: scheme-let->vals(let)
; Type: [Scheme-let -> LIST(Scheme-exp)]
; Purpose: Extract the values of all the bindings in the let
; Pre-conditions: -
; Tests:
; (scheme-let->vals (make-scheme-let (list (cons (make-scheme-var 'a) 2)) 
;                           (list (make-scheme-app '+ (list (make-scheme-var 'a) 3)))))
; --> '(2)
(define scheme-let->vals
  (lambda (let)
    (map cdr (scheme-let->bindings let))))

; Signature: scheme-let->body(let)
; Type: [Scheme-let -> LIST(Scheme-exp)]
; Purpose: Accessor 
; Pre-conditions: -
; Tests:
; (scheme-let->body (make-scheme-let (list (cons (make-scheme-var 'a) 2)) 
;                           (list (make-scheme-app '+ (list (make-scheme-var 'a) 3)))))
; --> '((app + (a 3)))
(define scheme-let->body
  (lambda (let)
    (second (tagged->content let))))

	
;; ------------------------------
;; SCHEME-LET*

;; Type: 
;; Example: (make-scheme-let* (list (cons (make-scheme-var 'a) 2)
;;                                  (cons (make-scheme-var 'b) 3))
;;                            (list (make-scheme-app '+ (list (make-scheme-var 'a) (make-scheme-var 'b)))))
;;          --> (let* ((a . 2) (b . 3)) ((app + (a b))))
;;

; Signature: make-scheme-let*(bindings, body)
; Type: [LIST(Pair(Scheme-var, Scheme-exp)) * LIST(Scheme-exp) -> Scheme-let*]
; Purpose: Value constructor
; Pre-conditions: -
; Tests:
; (make-scheme-let* (list (cons (make-scheme-var 'a) 2)
;                                  (cons (make-scheme-var 'b) 3))
;                            (list (make-scheme-app '+ (list (make-scheme-var 'a) (make-scheme-var 'b)))))
; --> '(let* ((a . 2) (b . 3)) ((app + (a b))))
(define make-scheme-let*
  (lambda (bindings body)
    (make-tagged 'let* (list bindings body))))

(define scheme-let*? (tagged-by? 'let*))

; Signature: scheme-let*->bindings(let*)
; Type: [Scheme-let* -> LIST(Pair(Scheme-var, Scheme-exp))]
; Purpose: Accessor
; Pre-conditions: -
; Tests: -
(define scheme-let*->bindings
  (lambda (let*)
    (first (tagged->content let*))))

; Signature: scheme-let*->vars(let*)
; Type: [Scheme-let* -> LIST(Scheme-var)]
; Purpose: Extract the vars in the bindings of a let* expression
; Pre-conditions: -
; Tests: -
(define scheme-let*->vars
  (lambda (let*)
    (map car (scheme-let*->bindings let*))))

; Signature: scheme-let*->vals(let*)
; Type: [Scheme-let* -> LIST(Scheme-exp)]
; Purpose: Extract the values in the bindings of a let* expression
; Pre-conditions: -
; Tests: -
(define scheme-let*->vals
  (lambda (let*)
    (map cdr (scheme-let*->bindings let*))))

; Signature: scheme-let*->body(let*)
; Type: [Scheme-let* -> LIST(Scheme-exp)]
; Purpose: Accessor
; Pre-conditions: -
; Tests: -
(define scheme-let*->body
  (lambda (let*)
    (second (tagged->content let*))))

;; ------------------------------
;; SCHEME-LETREC

; Signature: make-scheme-letrec(bindings, body)
; Type: [LIST(Pair(Scheme-var, Scheme-exp)) * LIST(Scheme-exp) -> Scheme-letrec]
; Purpose: Value constructor for scheme-letrec
; Pre-conditions: -
; Tests: 
; (make-scheme-letrec (list (cons (make-scheme-var 'a) 
;                                 (make-scheme-proc
;                                   (list (make-scheme-var 'a))
;                                   (list (make-scheme-app '+ (list (make-scheme-var 'a) 2))))))
;                     (list (make-scheme-app 'a (list 3))))
; --> '(letrec ((a . (lambda (a) ((app + (a 2)))))) ((app a (3))))
(define make-scheme-letrec
  (lambda (bindings body)
    (make-tagged 'letrec (list bindings body))))

(define scheme-letrec? (tagged-by? 'letrec))

; Signature: scheme-letrec->bindings(letrec)
; Type: [Scheme-letrec -> LIST(Pair(Scheme-var, Scheme-exp))]
; Purpose: Accessor 
; Pre-conditions: -
; Tests: -
(define scheme-letrec->bindings
  (lambda (letrec)
    (first (tagged->content letrec))))

; Signature: scheme-letrec->vars(letrec)
; Type: [Scheme-letrec -> LIST(Scheme-var)]
; Purpose: Extract the list of variables that are bound in the bindings of the letrec
; Pre-conditions: -
; Tests: -
(define scheme-letrec->vars
  (lambda (letrec)
    (map car (scheme-letrec->bindings letrec))))

; Signature: scheme-letrec->vals(letrec)
; Type: [Scheme-letrec -> LIST(Scheme-exp)]
; Purpose: Extract the values of all the bindings in the letrec
; Pre-conditions: -
; Tests: -
(define scheme-letrec->vals
  (lambda (letrec)
    (map cdr (scheme-let->bindings letrec))))

; Signature: scheme-letrec->body(letrec)
; Type: [Scheme-letrec -> LIST(Scheme-exp)]
; Purpose: Accessor 
; Pre-conditions: -
; Tests: -
(define scheme-letrec->body
  (lambda (letrec)
    (second (tagged->content letrec))))

;; ------------------------------
;; SCHEME-IF

; Signature: make-scheme-if(test, then, else)
; Type: [Scheme-exp * Scheme-exp * Scheme-exp -> Scheme-if]
; Purpose: Value constructor for Scheme-if
; Pre-conditions: -
; Tests: 
; (make-scheme-if (make-scheme-app '> (list 3 2)) 3 2)
; --> '(if (app > (3 2)) 3 2)
(define make-scheme-if
  (lambda (test then else)
    (make-tagged 'if (list test then else))))

(define scheme-if? (tagged-by? 'if))

; Signature: scheme-if->test(if)
; Type: [Scheme-if -> Scheme-exp]
; Purpose: Accessor for component test in scheme-if
; Pre-conditions: -
; Tests: -
(define scheme-if->test
  (lambda (if)
    (first (tagged->content if))))

; Signature: scheme-if->then(if)
; Type: [Scheme-if -> Scheme-exp]
; Purpose: Accessor for component then in scheme-if
; Pre-conditions: -
; Tests: -
(define scheme-if->then
  (lambda (if)
    (second (tagged->content if))))

; Signature: scheme-if->else(if)
; Type: [Scheme-if -> Scheme-exp]
; Purpose: Accessor for component else in scheme-if
; Pre-conditions: -
; Tests: -
(define scheme-if->else
  (lambda (if)
    (third (tagged->content if))))



;; ============================================================
;; AST WALKER

;; ------------------------------
; Signature: scheme-composite->components(c)
; Type: [Scheme-composite -> List(Scheme-exp)]
; Purpose: Traverse composite expressions as a tree
;          This is a polymorphic function.
; Pre-conditions: -
; Tests: -
(define scheme-composite->components
  (lambda (c)
    (cond ((scheme-proc? c) (append (scheme-proc->vars c)
                                    (scheme-proc->body c)))
          ((scheme-app? c) (cons (scheme-app->operator c)
                                 (scheme-app->operands c)))
          ((scheme-let? c) (append (scheme-let->vars c)
                                   (scheme-let->vals c)
                                   (scheme-let->body c)))
          ((scheme-let*? c) (append (scheme-let*->vars c)
                                    (scheme-let*->vals c)
                                    (scheme-let*->body c)))
          ((scheme-letrec? c) (append (scheme-letrec->vars c)
                                    (scheme-letrec->vals c)
                                    (scheme-letrec->body c)))
          ((scheme-if? c) (list (scheme-if->test c)
                                (scheme-if->then c)
                                (scheme-if->else c)))
          (else (error 'scheme-composite->components
                       "Bad scheme abstract syntax ~s" c)))))


;; ============================================================
;; Scheme-Parser

; Signature: scheme-parse(e)
; Type: [Concrete-syntax -> Scheme-exp]  
; Purpose: construct the abstract syntax used by the type inference system
; Pre-conditions: -
; Tests: -
(define scheme-parse
  (lambda (e)
    (cond
      ((number? e) e)
      ((boolean? e) e)
      ((scheme-var? e) (make-scheme-var e))
      ((empty? e) (error 'scheme-parse "Unexpected empty"))
      ((pair? e)
       (cond ((eq? (car e) 'define)
              (make-scheme-define (make-scheme-var (second e))
                                  (scheme-parse (third e))))
             ((eq? (car e) 'lambda)
              (make-scheme-proc (map make-scheme-var (second e))
                                (map scheme-parse (cddr e))))
             ((eq? (car e) 'let)
              (let ((bindings (second e))
                    (body (cddr e)))
                (make-scheme-let (map (lambda (binding) 
                                        (cons (make-scheme-var (first binding))
                                              (scheme-parse (second binding))))
                                      bindings)
                                 (map scheme-parse body))))
             ((eq? (car e) 'let*)
              (let ((bindings (second e))
                    (body (cddr e)))
                (make-scheme-let* (map (lambda (binding) 
                                         (cons (make-scheme-var (first binding))
                                               (scheme-parse (second binding))))
                                       bindings)
                                  (map scheme-parse body))))
             ((eq? (car e) 'letrec)
              (let ((bindings (second e))
                    (body (cddr e)))
                (make-scheme-letrec (map (lambda (binding) 
                                           (cons (make-scheme-var (first binding))
                                                 (scheme-parse (second binding))))
                                         bindings)
                                    (map scheme-parse body))))
             ((eq? (car e) 'if)
              (let ((test (scheme-parse (second e)))
                    (then (scheme-parse (third e)))
                    (else (scheme-parse (fourth e))))
                (make-scheme-if test then else)))
             (else (make-scheme-app (scheme-parse (car e))
                                    (map scheme-parse (cdr e))))))
      (else (error 'scheme-parse "Unexpected type")))))

; Signature: scheme-concrete(e)
; Type: [Scheme-exp -> Concrete-syntax]
; Purpose: Inverse function of parse - returns the concrete syntax of an abstract syntax tree.
; Pre-conditions: -
; Tests: -
(define scheme-concrete
  (lambda (e)
    (cond ((scheme-atomic? e) e)
          ((scheme-define? e)
           (list 'define
                 (scheme-concrete (scheme-define->var e))
                 (scheme-concrete (scheme-define->val e))))
          ((scheme-proc? e)
           (cons 'lambda
                 (cons (map scheme-concrete (scheme-proc->vars e))
                       (map scheme-concrete (scheme-proc->body e)))))
          ((scheme-let? e)
           (cons 'let 
                 (cons (map list 
                            (map scheme-concrete (scheme-let->vars e))
                            (map scheme-concrete (scheme-let->vals e)))
                       (map scheme-concrete (scheme-let->body e)))))
          ((scheme-let*? e)
           (cons 'let* 
                 (cons (map list 
                            (map scheme-concrete (scheme-let*->vars e))
                            (map scheme-concrete (scheme-let*->vals e)))
                       (map scheme-concrete (scheme-let*->body e)))))
          ((scheme-letrec? e)
           (cons 'letrec
                 (cons (map list 
                            (map scheme-concrete (scheme-letrec->vars e))
                            (map scheme-concrete (scheme-letrec->vals e)))
                       (map scheme-concrete (scheme-letrec->body e)))))
          ((scheme-if? e)
           (list 'if
                 (scheme-concrete (scheme-if->test e))
                 (scheme-concrete (scheme-if->then e))
                 (scheme-concrete (scheme-if->else e))))
          ((scheme-app? e)
           (cons (scheme-concrete (scheme-app->operator e))
                 (map scheme-concrete (scheme-app->operands e))))
          (else (error 'scheme-concrete "Bad scheme abstract syntax ~s" e)))))
