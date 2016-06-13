#lang racket

(require "./env-interpreter-compiler/analyzer-core.rkt" "asp.rkt")

(run-tests
 (test (derive-analyze-eval '(and 1 2 3)) => '3)
 (test (derive-analyze-eval '(and)) => '#t)
 (test (derive-analyze-eval '(and 1 #f 3)) => '#f)
 (test (derive-analyze-eval '(and 1 #f (/ 1 0))) => '#f)
 (test (derive-analyze-eval '(and 1 ((lambda (x)(= x 5)) 4))) => '#f)
 (test (derive-analyze-eval '(and 1 ((lambda (x)(= x 5)) 5) 5)) => '5)
 (test (derive-analyze-eval '(and 1 ((lambda (x)(= x 5)) 5) (((lambda()(lambda()#f)))) 5)) => '#f)
 (test (derive-analyze-eval '(and 1 ((lambda (x)(= x 5)) 5) ((lambda()(lambda()#f)))4)) => 4)
 (test (derive-analyze-eval '(and #f)) => '#f)
 (test (derive-analyze-eval '(and (= 1 1))) => #t)
 (test (derive-analyze-eval '(and (= 1 2))) => #f)
 (test (derive-analyze-eval '(and 4 4 4 5 5 5 #t (= 1 2))) => #f)
 
 )