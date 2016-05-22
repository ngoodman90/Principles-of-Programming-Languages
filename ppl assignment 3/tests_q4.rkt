#lang racket

(require "utils.rkt")
(require "q4.rkt")

(define l1 '((a . 1) (b . 2) (c . 3)))
(define l2 '((d . 4) (e . 5) (e . 6)))
(define l3 '((g . 7) (e . 8) (h . 9)))

(define odd?$
  (lambda (n c)
    (c (odd? n))))

;; q4 tests
(define (q4-tests)
  (run-tests
   
   ; filter1$
   (test (filter1$ odd?$
                   '(1 2 3 4 5)
                   (lambda(ls) (foldr + 0 ls)))
         => 9)
   
   ; filter2$
   (test (filter2$ even?
                   '(1 2 3 4 5)
                   (lambda(ls) (foldr + 0 ls)))
         => 6)
   
   ; get-first-value
   (test (get-first-value (list l1 l2 l3) 'e) => 5)
   (test (get-first-value (list l1 l2 l3) 'g) => 7)
   (test (get-first-value (list l1 l2 l3) 'k) => '())
   
   ; get-first-value
   (test (collect-all-values (list l2 l3) 'e) => '(5 8))
   (test (collect-all-values (list l2 l3) 'k) => '())
   
  ))

(display "q4-tests:\t\t")
(q4-tests)


