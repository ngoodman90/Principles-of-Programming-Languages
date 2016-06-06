#lang racket

(require "./substitution-interpreter/substitution-core.rkt" "asp.rkt")

(run-tests
   (test (derive-eval '(defined? x)) => '(value #f))
   (test (derive-eval '(define x #f)) => 'ok)
   (test (derive-eval '(defined? x)) => '(value #t))
   (test (derive-eval '(if (defined? x) x 0)) => '(value #f))
)