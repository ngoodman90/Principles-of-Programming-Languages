#lang racket

(require "./env-interpreter-compiler/interpreter-core.rkt" "asp.rkt")

(run-tests
 (test
  (derive-eval 
   '(define foo (lambda (x) (lambda(y) (+ x y)))))
  => 'ok)
 (test
  (derive-eval 
   '(defined-in-closure? x (foo 5)))
  => '#t)
 (test
  (derive-eval 
   '(defined-in-closure? y (foo 5)))
  => '#f)
 )