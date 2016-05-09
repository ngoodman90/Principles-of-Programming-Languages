#lang racket

(require "utils.rkt"
         "auxiliary.rkt"
         "type-expression-adt.rkt"
         "type-expression-parser.rkt"
         "solve.rkt"
         "equation-adt.rkt"
         "substitution-adt.rkt"
         "scheme-ast.rkt"
         "scheme-exp-to-pool.rkt"
         "scheme-exp-to-equations.rkt")


(require srfi/1)
(provide (all-defined-out))

;; Support exception handling
(define (try f (ans 'error))
  (with-handlers ((exn?
                   (lambda (exn) ans))) (f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Signature: verify-te-of-expr(expr te)
;; Purpose:   (1) Map type-vars to sub expressions in expr (expr-tvars-list).
;;            (2) Let tvar-of-expr be the type-var associated with expr.
;;            (3) Generate type equations based on the expr-tvar map.
;;            (4) Solve the equations.
;;            (5) Find the type-expression associated with tvar-of-expr.
;;            (6) See if it is equivalent to the given expression, expr.
;;
(define verify-te-of-expr
  (lambda (exp tec)
    (let* ((expr (scheme-parse exp))
           (te (te-parse tec))
           (pool (scheme-exp->pool expr))
           (equations  (pool->equations pool))
           (solve-sub  (solve-equations equations))
           (tvar-of-expr (val-of-index expr pool))
           (type-of-expr (sub-apply solve-sub tvar-of-expr)))
      (equivalent-tes? te type-of-expr))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Demo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Below is a step-by-step demonstration of how type expressions are
;; verified for a given language expression. They describe the steps
;; made in the procedure verify-te-of-expr above. Uncomment to display.
;;
;; (define expr (scheme-parse '(lambda (x) (+ x 1))))
;; (define te (te-parse '(-> (* Number) Number)))
;; (fprintf (current-output-port)
;;   "Demo: Verify the type ~a for the expression ~a\n" te expr)
;; (fprintf (current-output-port)
;;   "----------------------------------------\n")
;; (define pool (scheme-exp->pool expr))
;; (fprintf (current-output-port) "1. Map type vars to each sub-expr:\n")
;; (fprintf (current-output-port) "   ~a\n\n" pool)
;; (define tvar-of-expr (val-of-index expr pool))
;; (fprintf (current-output-port)
;;   "2. The type var associated with the given expr is ~a\n\n" tvar-of-expr)
;; (define equations (pool->equations pool))
;; (fprintf (current-output-port)
;;   "3. Generated equations for all sub exprs:\n")
;; (fprintf (current-output-port) "   ~a\n\n" equations)
;; (define solve-sub (solve equations (make-empty-sub)))
;; (fprintf (current-output-port) "4. Solving the equations, we get:\n")
;; (fprintf (current-output-port) "   ~a\n\n" solve-sub)
;; (define type-of-expr (sub-apply solve-sub tvar-of-expr))
;; (fprintf (current-output-port)
;;   "5. The type expression associated with ~a is: ~a\n\n"
;;   tvar-of-expr type-of-expr)
;; (fprintf (current-output-port)
;;   "It now remains to verify that the inferred type expression is equivalent to the given one.\n")
;;(fprintf (current-output-port)
;;  "----------------------------------------\n\n")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define unifyable-structure-tests
  (lambda ()
    (display "unifyable-structure-tests:\t")
    (run-tests
     ;; Same type constructor
     (test (unifyable-structure
            (make-equation '(-> (* T_3) T_1)
                           '(-> (* T_2) T_1))) => #t)

     ;; type-variable vs procedure-te
     (test (unifyable-structure
            (make-equation 'T_0 '(-> (* T_3) T_1))) => #f)

    )))


(define solve-tests
  (lambda ()
    (display "solve-tests:\t")
    (run-tests


     ;; Equations: T1 = T3, Substitution: Empty
     (test (solve (list (make-equation 'T_1 'T_3))
                  (make-empty-sub))
           => (make-sub '(T_1) '(T_3)))

     (test (solve (list (make-equation 'T_1 'T_3))
                  (make-sub '(T_1) '(Number)))
           => (make-sub '(T_3 T_1) '(Number Number)))

     (test (verify-te-of-expr 3 'Number)
           => #t)
     (test (verify-te-of-expr '(+ 1 2) 'Number))
     (test (verify-te-of-expr '(+ (+ 1 2) 3) 'Number))
     (test (verify-te-of-expr '+ '[Number * Number -> Number]))
     (test (verify-te-of-expr '> '[Number * Number -> Boolean]))
     (test (verify-te-of-expr '(> 1 2) 'Boolean))
     (test (verify-te-of-expr '(> 1 (+ 1 2)) 'Boolean))

     ;; Type of '(lambda (x) (+ x 1)) is '(-> (* Number) Number)
     (test (verify-te-of-expr
            '(lambda (x) (+ x 1))
            '[Number -> Number])
           => #t)

     ;; Type of '((lambda (x) (+ x 1)) 3) is 'Number
     (test (verify-te-of-expr
            '((lambda (x) (+ x 1)) 3)
            'Number)
           => #t)

     ;; Verify that the type of '((lambda (x) (+ x 1)) 3) is equivalent
     ;; to `(-> (* (-> (* Number) T)) T), where T is some type variable
     (test (verify-te-of-expr
            '(lambda (x) (x 11))
            '[(Number -> T) -> T])
           => #t)

     ;; g: [T1->T2]
     ;; f: [T2->T3]
     ;; ==> (lambda(n) (f (g n)))               : [T1->T3]
     ;; ==> (lambda(f g) (lambda(n) (f (g n)))) : [[T2-T3]*[T1->T2]->[T1->T3]]
     (test (verify-te-of-expr
            '(lambda (f g) (lambda (n) (f (g n))))
            '[(T2 -> T3) * (T1 -> T2) -> (T1 -> T3)])
           => #t)

     ;; f: [N->N]
     ;; ==> (lambda(x) (- (f 3) (f x)))             : [N->N]
     ;; ==> (lambda(f) (lambda(x) (- (f 3) (f x)))) : [[N->N]->[N->N]]
     (test (verify-te-of-expr
            '(lambda (f) (lambda (x) (- (f 3) (f x))))
            '[(Number -> Number) -> Number -> Number])
           => #t)

     (test (verify-te-of-expr
            '(lambda (x) (+ (+ x 1) (+ x 1)))
            '[Number -> Number])
           => #t)

     (test (verify-te-of-expr
            '(lambda () (lambda (x) (+ (+ x 1) (+ x 1))))
            '[Empty -> Number -> Number])
           => #t)

     (test (verify-te-of-expr
            '((lambda (x) (x 11 3)) +)
            'Number)
           => #t)

     ;; (lambda (y) y):      [T1 -> T1]
     ;; (lambda (x) (x 11)): [N  -> T2]
     (test (verify-te-of-expr
            '((lambda (x) (x 11)) (lambda (y) y))
            'Number)
           => #t)

     ;; Circular types cannot be inferred 
     (test (try
            (lambda ()
              (verify-te-of-expr
               '(lambda (x) (x x))
               'Circular)))
           => 'error)

     ;; A free variable cannot have type inferred
     (test (verify-te-of-expr 'x 'T)
           => #t)

     ;; A free variable whose type is inferred from context
     (test (verify-te-of-expr '(+ x 1) 'Number)
           => #t)

     ;; Not enough info in context to infer type of f
     (test (verify-te-of-expr '(f 1) 'T)
           => #t)

     ;; Primitive provides sufficient info
     (test (verify-te-of-expr '(> (f 1) 0) 'Boolean)
           => #t)

     ;; Parameters that are not used
     (test (verify-te-of-expr '(lambda (x) 1) '(T -> Number))
           => #t)
     (test (verify-te-of-expr '(lambda (x y) x) '(T1 * T2 -> T1))
           => #t)
     (test (verify-te-of-expr '((lambda (x) 1) 2) 'Number)
           => #t)

     ;; Bad number of parameters
     ;; Extra param
     (test
      (try
       (lambda ()
         (verify-te-of-expr '((lambda () 1) 2) 'bad-arity)))
       => 'error)
     ;; Missing param
     (test
      (try
       (lambda ()
         (verify-te-of-expr '((lambda (x) 1))) 'bad-arity))
      => 'error)
     
     )))

(define split-equation-tests
  (lambda ()
    (display "split-equation-tests:\t")
    (run-tests

     (test (equal?
            (split-equation
             (make-equation (make-tuple-te (list))
                            (make-tuple-te (list))))
            '()) => #t)

     (test (or (set=?
                (list->set
                 (split-equation (make-equation '(-> (* T_3) Number)
                                                '(-> (* Boolean) T_2))))
                (list->set (list (make-equation 'Number 'T_2)
                                 (make-equation 'T_3 'Boolean))))
               (set=?
                (list->set
                 (split-equation (make-equation '(-> (* T_3) Number)
                                                '(-> (* Boolean) T_2))))
                (list->set (list (make-equation 'T_2 'Number)
                                 (make-equation 'Boolean 'T_3)))))
           => #t)

     (test (or (set=?
                (list->set
                 (split-equation
                  (make-equation '(-> (* T_3) (-> (* T_3) T_1))
                                 '(-> (* T_3) (-> (* T_2) T_2)))))
                (list->set
                 (list (make-equation '(-> (* T_3) T_1)
                                      '(-> (* T_2) T_2))
                       (make-equation 'T_3 'T_3))))
               (set=?
                (list->set
                 (split-equation
                  (make-equation '(-> (* T_3) (-> (* T_3) T_1))
                                 '(-> (* T_3) (-> (* T_2) T_2)))))
                (list->set
                 (list (make-equation '(-> (* T_2) T_2)
                                      '(-> (* T_3) T_1))
                       (make-equation 'T_3 'T_3)))))
           => #t)

     (test (or (set=?
                (list->set
                 (split-equation
                  (make-equation
                   '(-> (* T_3 Boolean Number) (-> (* T_3) Number))
                   '(-> (* T_3 T_2 Number)     (-> (* Boolean) T_5)))))
                (list->set
                 (list (make-equation '(-> (* T_3) Number)
                                      '(-> (* Boolean) T_5))
                       (make-equation 'T_3 'T_3)
                       (make-equation 'Boolean 'T_2)
                       (make-equation 'Number 'Number))))
               (set=?
                (list->set
                 (split-equation
                  (make-equation
                   '(-> (* T_3 Boolean Number) (-> (* T_3) Number))
                   '(-> (* T_3 T_2 Number)     (-> (* Boolean) T_5)))))
                (list->set
                 (list (make-equation '(-> (* Boolean) T_5)
                                      '(-> (* T_3) Number))
                       (make-equation 'T_3 'T_3)
                       (make-equation 'T_2 'Boolean)
                       (make-equation 'Number 'Number)))))
           => #t)

     )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Invoke
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unifyable-structure-tests)
(solve-tests)
(split-equation-tests)
