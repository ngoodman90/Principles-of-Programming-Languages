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
  (test
  (derive-eval 
   '(define foo2 (lambda (x y) (lambda (z) (lambda (a) x)))))
  => 'ok)
 (test
  (derive-eval 
   '(defined-in-closure? x ((foo2 1 2) 3)))
  => '#t)
  (test
  (derive-eval 
   '(defined-in-closure? y ((foo2 1 2) 3)))
  => '#t)
  (test
  (derive-eval 
   '(defined-in-closure? z ((foo2 1 2) 3)))
  => '#t)
 (test
  (derive-eval 
   '(defined-in-closure? a ((foo2 1 2) 3)))
  => '#f)
  (test
  (derive-eval 
   '(defined-in-closure? b ((foo2 1 2) 3)))
  => '#f)
  (test
  (derive-eval 
   '(define foo3 (lambda() (lambda(x) 1))))
  => 'ok)  
  (test
    (derive-eval 
   '(defined-in-closure? x (foo3)))
  => '#f)
 )