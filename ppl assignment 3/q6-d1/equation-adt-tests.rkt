#lang racket

(require "utils.rkt"
         "type-expression-adt.rkt"
         "scheme-ast.rkt"
         "equation-adt.rkt"
         "scheme-exp-to-pool.rkt"
         "scheme-exp-to-equations.rkt")

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define se1 (scheme-parse '(lambda (x) (+ x 1))))
(define se11 (scheme-parse '(+ x 1)))

(define pool1 `( (,se1  var_0)
                           (,se11 var_1)
                           (1     var_4)
                           (x     var_3)
                           (+     var_2)))

'((var_0 (-> (* var_3) var_1))
  (var_2 (-> (* var_3 var_4) var_1))
  (var_4 Number)
  (var_2 (-> (* Number Number) Number)))

(define se2 (scheme-parse '((lambda (f x) (f x)) sqrt 4)))
(define se21 (scheme-parse '(lambda (f x) (f x))))
(define se22 (scheme-parse '(f x)))
(define pool2 `( (,se2  var_0)
                           (,se21 var_1)
                           (,se22 var_2)
                           (x     var_3)
                           (f     var_4)
                           (sqrt  var_5)
                           (4     var_6)))

(define se3 (scheme-parse
             '((lambda (f g) (lambda (x) (f (+ x (g 3))))) inc sqr)))
(define se31 (scheme-parse '(lambda (f g) (lambda (x) (f (+ x (g 3)))))))
(define se32 (scheme-parse '(lambda (x) (f (+ x (g 3))))))
(define se33 (scheme-parse '(f (+ x (g 3)))))
(define se34 (scheme-parse '(+ x (g 3))))
(define se35 (scheme-parse '(g 3)))
  (define pool3
    `( (,se3  var_0)   ; (var_1 (-> (* var_11 var_12) var_0))
       (,se31 var_1)   ; (var_1 (-> (* var_6 var_9) var_2))
       (,se32 var_2)   ; (var_2 (-> (* var_8) var_3))
       (,se33 var_3)   ; (var_6 (-> (* var_4) var_3))
       (,se34 var_4)   ; (var_7 (-> (* var_8 var_5) var_4))
       (,se35 var_5)   ; (var_9 (-> (* var_10) var_5))
       (f     var_6)
       (+     var_7)
       (x     var_8)
       (g     var_9)
       (3     var_10)
       (inc   var_11)
       (sqr   var_12)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define tests
  (lambda ()
    (display "tests:")
    (run-tests

     (test (make-equation 'T1 'Boolean) => '(equation T1 Boolean))
     (test (make-equation
            (make-proc-te (make-tuple-te '(T1)) 'T2)
            (make-proc-te (make-tuple-te '(Boolean)) 'Number))
           => '(equation (-> (* T1) T2) (-> (* Boolean) Number)))

     (test (let ((equations (pool->equations pool1)))
             (not (and
                   ;; Eq generated for se1 (procedure)
                   (member '(equation var_0 (-> (* var_3) var_1)) equations)
                   ;; For se2 (application)
                   (member '(equation var_2 (-> (* var_3 var_4) var_1)) equations))))
           => #f)

     (test (let ((equations (pool->equations pool2)))
             (not (and
                   ;; Eq generated for se 1 (application)
                   (member '(equation var_1 (-> (* var_5 var_6) var_0)) equations)
                   ;; Eq generated for se 2 (procedure)
                   (member '(equation var_1 (-> (* var_4 var_3) var_2)) equations)
                   ;; Eq generated for se 3 (application)
                   (member '(equation var_4 (-> (* var_3) var_2)) equations)
              )))
           => #f)

     ; See the manually generated equations next to the definition of pool3
     (test (let ((equations (pool->equations pool3)))
             (not (and
                   (member '(equation var_1 (-> (* var_11 var_12) var_0)) equations)
                   (member '(equation var_1 (-> (* var_6 var_9) var_2)) equations)
                   (member '(equation var_6 (-> (* var_4) var_3)) equations)
                   (member '(equation var_7 (-> (* var_8 var_5) var_4)) equations)
                   (member '(equation var_9 (-> (* var_10) var_5)) equations)
              )))
           => #f)

  )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Invoking tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tests)
