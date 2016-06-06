#lang racket

(require "./env-interpreter-compiler/analyzer-core.rkt" "asp.rkt")

(run-tests
 (test (derive-analyze-eval '(and 1 2 3)) => '3)
 (test (derive-analyze-eval '(and)) => '#t)
 (test (derive-analyze-eval '(and 1 #f 3)) => '#f)
 (test (derive-analyze-eval '(and 1 #f (/ 1 0))) => '#f)
 )