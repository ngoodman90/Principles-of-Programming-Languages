#lang racket

(require "./substitution-interpreter/substitution-core.rkt" "asp.rkt")

(run-tests
 (test (derive '(and 3)) => '3)
 (test (derive '(and a b c)) => '(if a (if b c #f) #f))
 (test (derive-eval '(and 3 4)) => '(value 4))
 (test (derive-eval '(and #f 4)) => '(value #f))
 (test (derive-eval '(and 3)) => '(value 3))
)