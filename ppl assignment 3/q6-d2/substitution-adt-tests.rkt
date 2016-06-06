#lang racket

;; Load tests runner file.
(require "utils.rkt")

;; Additional required files
(require "type-expression-adt.rkt"
         "type-expression-parser.rkt"
         "substitution-adt.rkt")

(provide (all-defined-out))

;; Support exception handling
(define (try f (ans 'error))
  (with-handlers ((exn?
                   (lambda (exn) ans))) (f)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The empy substitution
(define sub0 (make-sub (list) (list)))

;; {T1:Number, T2:[T4 -> Number], T3:T9}
(define sub1
  (make-sub '(T1 T2 T3)
            (map te-parse
                 '(Number [T4 -> Number] T9))))

;; {T4:[T1->Number], T5:Boolean, T6:T7}
(define sub2
  (make-sub '(T4 T5 T6)
            (map te-parse
                 '([T1 -> Number]
                   Boolean
                   T7))))

;; {T7:Number, T8:[T5 * Number -> T3], T9:Boolean}
(define sub3
  (make-sub '(T7 T8 T9)
            (map te-parse
                 '(Number [T5 * Number -> T3] Boolean))))

;; {T1:Boolean, T2:[Number * T10], T3:[T5 * Boolean -> Number]}
(define sub4
  (make-sub '(T1 T2 T3)
            (map te-parse
                 '(Boolean [Number * T10] [T5 * Boolean -> Number]))))

;; {T1:Boolean, T2:(T5*Boolean), T3:Number}
(define sub5
  (make-sub '(T1 T2 T3)
            (map te-parse
                 '(Boolean [T5 * Boolean] Number))))

(define sub-to-vars-te-list
  (lambda (sub)
    (let ((vars (sub->variables sub))
          (tes (sub->tes sub)))
      (map list vars tes))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define sub-combination-tests
  (lambda ()
    (display "sub-combination-tests:\t")
    (run-tests

     ;; {T1:(* Number S1) T2:(-> (* Number) S4)} O {T3:(-> (* Number) S2)}
     (test
      (let ((vars-tes
             (sub-to-vars-te-list
              (sub-combine
               (make-sub '(T1 T2)
                         (map te-parse
                              '([Number * S1] [Number -> S4])))
               (make-sub '(T3)
                         (map te-parse
                              '([Number -> S2])))))))
        (set=? (list->set vars-tes)
               (list->set '((T3 (-> (* Number) S2))
                            (T1 (* Number S1))
                            (T2 (-> (* Number) S4))))))
      => #t)

     ;; {T1:(* Number S1), T2:(-> (* Number) T5)} O
     ;; {T3:(-> (* Number) S2), T4:(* Number S1), T5:Boolean}
     (test
      (let
          ((vars-tes
            (sub-to-vars-te-list
             (sub-combine
              (make-sub '(T1 T2)
                        (map te-parse
                             '([Number * S1]
                               [Number -> T5])))
              (make-sub '(T3 T4 T5)
                        (map te-parse
                             '([Number -> S2]
                               [Number * S1]
                               Boolean)))))))
        (set=? (list->set vars-tes)
               (list->set '((T3 (-> (* Number) S2))
                            (T1 (* Number S1))
                            (T2 (-> (* Number) Boolean))
                            (T4 (* Number S1))
                            (T5 Boolean)))))
           => #t)

     ;; {T1:(* Number S1), T2:(-> (* T5) T4)} O
     ;; {S1:Boolean, T3:(-> (* Number) S2), T5:(-> (* Number) S1), T4:Boolean}
     (test
      (let
          ((vars-tes
            (sub-to-vars-te-list
             (sub-combine
              (make-sub '(T1 T2)
                        (map te-parse
                             '([Number * S1] [T5 -> T4])))
              (make-sub '(S1 T3 T5 T4)
                        (map te-parse
                             '(Boolean
                               [Number -> S2]
                               [Number -> S1]
                               Boolean)))))))
        (set=? (list->set vars-tes)
               (list->set '((T3 (-> (* Number) S2))
                            (T1 (* Number Boolean))
                            (T2 (-> (* (-> (* Number) S1)) Boolean))
                            (T5 (-> (* Number) S1))
                            (T4 Boolean)
                            (S1 Boolean)))))
           => #t)

     ;; {T1:S1, T2:(* S2 Number), T3:Boolean} O
     ;; {S1:(* T5 (-> (* Number T2) T2), S2:T3}
     (test
      (let ((vars-tes
             (sub-to-vars-te-list
              (sub-combine
               (make-sub '(T1 T2 T3)
                         (map te-parse '(S1 [S2 * Number] Boolean)))
               (make-sub '(S1 S2)
                         (map te-parse
                              '([T5 * (Number * T2 -> T2)] T3)))))))
        (set=? (list->set vars-tes)
               (list->set '((T1 (* T5 (-> (* Number T2) T2)))
                            (T2 (* T3 Number))
                            (T3 Boolean)
                            (S1 (* T5 (-> (* Number T2) T2)))
                            (S2 T3)))))
           => #t)

     ;; {T1: Number, T2:(* T4 Number), T3:T9} o
     ;; {T4:(-> (* T1) Number), T5:Boolean, T6:T7}
     (test
      (let ((vars-tes
             (sub-to-vars-te-list
              (sub-combine
               (make-sub '(T1 T2 T3)
                         (map te-parse '(Number [T4 * Number] T9)))
               (make-sub '(T4 T5 T6)
                         (map te-parse '([T1 -> Number] Boolean T7)))))))
        (set=? (list->set vars-tes)
               (list->set '((T6 T7)
                            (T5 Boolean)
                            (T4 (-> (* T1) Number))
                            (T1 Number)
                            (T2 (* (-> (* T1) Number) Number))
                            (T3 T9)))))
      => #t)

     ;; Assert error on cicular substitution
     ;; {T3:Boolean S1:[Number -> T2] T4:[Number * S1] T5:Boolean} O
     ;; {T1:[Number * S1] T2:[T3 -> S1]}
     (test
      (try
       (lambda()
         (sub-combine
          (make-sub '(T3 S1 T4 T5)
                    (map te-parse
                         '(Boolean [Number -> T2] [Number * S1] Boolean)))
          (make-sub '(T1 T2)
                    (map te-parse
                         '([Number * S1] [T3 -> S1]))))))
      => 'error)

     ;; Assert T8 is removed after applying sub2 to sub1
     ;; {T7:Number, T8:(-> (* T5 Number) T3)} o {T5:T7, T8:Boolean}
     (test
      (let
          ((vars-tes
            (sub-to-vars-te-list
             (sub-combine
              (make-sub '(T7 T8)
                        (map te-parse
                             '(Number [T5 * Number -> T3])))
              (make-sub '(T5 T8)
                        (map te-parse
                             '(T7 Boolean)))))))
        (set=? (list->set vars-tes)
               (list->set '((T5 T7)
                            (T7 Number)
                            (T8 (-> (* T7 Number) T3))))))
      => #t)

     ;; MIRA, I'm not sure this one should produce an error.
     ;; Verify that identity binding is removed
     ;; {T7:Number, T8:T3} o {T5:T7, T3:T8}
     ;; (test (let ((vars-tes (sub-to-vars-te-list
     ;;                       (substitution-combination
     ;;                        (make-sub '(T7 T8) (list 'Number 'T3))
     ;;                        (make-sub '(T5 T3) (list 'T7 'T8)))
     ;;                       )))
     ;;        (set=? (list->set vars-tes)
     ;;               (list->set '((T7 Number)
     ;;                            (T5 T7)))
     ;;             ))
     ;;      => #t)

     )))


(define extend-sub-tests
  (lambda ()
    (display "extend-sub-tests:\t")
    (run-tests

     (test
      (let ((vars-tes
             (sub-to-vars-te-list
              (extend-sub
               (make-sub '(T1 T2 T3)
                         (map te-parse '(S1 [S2 * Number] Boolean)))
               'S1
               (te-parse '[T21 * (Number * T23 -> T22)])))))
        (set=? (list->set vars-tes)
               (list->set '((S1 (* T21 (-> (* Number T23) T22)))
                            (T1 (* T21 (-> (* Number T23) T22)))
                            (T2 (* S2 Number))
                            (T3 Boolean)))))
           => #t)

     ;; Assert error due to circular substitution
     (test
      (try
       (lambda()
         (extend-sub
          (make-sub '(T1 T2 T3)
                    (map te-parse '(S1 [S2 * Number] Boolean))
                   'S1
                   (te-parse '[T1 * (Number * T23 -> T22)])))))
      => 'error)

     ;; Assert error due to circular substitution
     (test
      (try
       (lambda()
         (extend-sub
          (make-sub '(T1 T2 T3)
                    (map te-parse '(S1 [S2 * Number] Boolean)))
          'S1
          (te-parse '[S1 * (Number * T23 -> T22)]))))
      => 'error)

     )))


(define sub-equal-tests
  (lambda ()
    (display "sub-equal-tests:\t")
    (run-tests

     (test
      (sub-equal?
       (make-sub '(x y z)
                 (map te-parse
                      '(Number Boolean [Number -> Number])))
       (make-sub '(y x z)
                 (map te-parse
                      '(Boolean Number [Number -> Number]))))
      => #t)

     (test (sub-equal? (make-empty-sub) (make-empty-sub))
           => #t)

     )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Invoking tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(sub-combination-tests)
(extend-sub-tests)
(sub-equal-tests)
