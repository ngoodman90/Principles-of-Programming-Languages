#lang racket

;; Load tests runner file.
(require "utils.rkt")

;; Load the file with functions to be tested.
(require "type-expression-adt.rkt"
         "type-expression-parser.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; [Empty -> Number]
(define te1 (make-proc-te
             (make-empty-tuple-te)
             'Number))


;; [Number -> Boolean]
(define te2 (make-proc-te
             (make-tuple-te (list 'Number))
             'Boolean))

;; [Boolean * Number -> [Number->Boolean]]
(define te3 (make-proc-te
             (make-tuple-te (list 'Boolean 'Number))
             (make-proc-te (make-tuple-te
                            (list 'Number))
                           'Boolean)))

;; [[T1 -> Number] * Number -> Boolean]
(define te4 (make-proc-te
             (make-tuple-te (list (make-proc-te
                                   (make-tuple-te '(T1))
                                   'Number)
                                  'Number))
             'Boolean))

;; [[T1 -> Number] * [T2 -> Boolean] -> [T1 * T2 -> [Number -> Boolean]]]
(define te5 (make-proc-te
             (make-tuple-te (list (make-proc-te
                                   (make-tuple-te '(T1)) 'Number)
                                  (make-proc-te
                                   (make-tuple-te '(T2)) 'Boolean)))
             (make-proc-te
              (make-tuple-te '(T1 T2)) (make-proc-te
                                        (make-tuple-te '(Number))
                                        'Boolean))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define test-te-parse
  (lambda ()
    (run-tests
     (test (te-parse '[Empty -> Number]) => te1)
     (test (te-parse '[Number -> Boolean]) => te2)
     (test (te-parse '[Boolean * Number -> [Number -> Boolean]]) => te3)
     (test (te-parse '[[T1 -> Number] * Number -> Boolean]) => te4)
     (test (te-parse '[[T1 -> Number] * [T2 -> Boolean] -> [T1 * T2 -> [Number -> Boolean]]]) => te5)
     )))

(define test-te-adt
  (lambda ()
    (run-tests

     ;; te1 = [Empty -> Number]
     (test (proc-te? te1) => #t)
     (test (proc-te->parameter-components te1) => (list))
     (test (proc-te->return te1) => 'Number)

     ;; te2 = [Number -> Boolean]
     (test (proc-te? te2) => #t)
     (test (proc-te->parameters te2) => '(* Number))
     (test (proc-te->parameter-components te2) => '(Number))
     (test (proc-te->return te2) => 'Boolean)
     (test (high-order-te? te2) => #f)

     ;; te3 = [Boolean * Number -> [Number->Boolean]]
     (test (proc-te? te3) => #t)
     (test (proc-te->parameters te3) => '(* Boolean Number))
     (test (proc-te->parameter-components te3) => '(Boolean Number))
     (test (polymorphic-te? te3) => #f)
     (test (high-order-te? te3) => #t)
     (test (proc-te? (proc-te->return te3)) => #t)
     (test (proc-te->return te3) => '(-> (* Number) Boolean))

     ;; te4 = [[T1 -> Number] * Number -> Boolean]
     (test (proc-te->parameters te4) => '(* (-> (* T1) Number) Number))
     (test (proc-te->parameter-components te4) => '((-> (* T1) Number) Number))
     (test (polymorphic-te? te4) => #t)
     (test (high-order-te? te4) => #t)
     (test (proc-te? (proc-te->return te4)) => #f)

     ;; te5 = [ [T1->Number]*[T2->Boolean] -> [T1*T2->[Number->Boolean]] ]
     (test (polymorphic-te? te5) => #t)
     (test (high-order-te? te5) => #t)
     (test (polymorphic-te? (proc-te->return te5)) => #t)

     )))

(define low-level-tests
  (lambda ()
    (display "low-level-tests:")
    (run-tests

     (test (te? '()) => #f)
     (test (andmap te? `(T2
                         Number
                         (*)
                         (* Boolean Number)
                         (-> (* T2) Boolean)
                         ,te1))
           => #t)


     (test (atomic-te? '()) => #f)
     (test (map atomic-te? `(T2
                                 Number
                                 (*)
                                 (* Boolean Number)
                                 (-> (* T2) Boolean)
                                 ,te3))
              => '(#f #t #f #f #f #f))


     (test (tuple-te? '()) => #f)
     (test (map tuple-te? `(T2
                            Number
                            (*)
                            empty-tuple-te
                            (* Boolean Number)
                            (-> (* T2) Boolean)
                            ,te5))
           => '(#f #f #t #t #t #f #f))


     (test (proc-te? '()) => #f)
     (test (map proc-te? `(T2
                               Number
                               (*)
                               (* Boolean Number)
                               (-> (* T2) Boolean)
                               ,te5))
           => '(#f #f #f #f #t #t))


     (test (map type-variable? `(()
                                 T2
                                 Number
                                 (*)
                                 (* Boolean Number)
                                 (-> (* T2) Boolean)
                                 ,te5))
           => '(#f #t #f #f #f #f #f))

     (test (map polymorphic-te? `(T2
                                  Number
                                  (*)
                                  (* Boolean Number)
                                  (-> (* T2) Boolean)
                                  ,te5))
           => '(#t #f #f #f #t #t))
     )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Invoke tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-te-parse)
(test-te-adt)
(low-level-tests)
