#lang racket

(require "utils.rkt")
(require "q2.rkt")

; Signature: nth-deriv(f,n)
; Type: [[Number -> Number]*Number -> [Number -> Number]
(define nth-deriv
  (lambda (f n)
    (if (= n 0)
        f
        (nth-deriv (derive f) (- n 1)))
    ))

(define f
  (lambda (x)
    (+ (expt x 3) (expt x 2))))

(define g
  (lambda (x)
    (+ (- (expt x 3) (* 2 x)) 2)))

;; q2 tests
(define (q2-tests)
  (run-tests
   
   ; c-nth-deriv-f(f)
   (test (((c-nth-deriv-f f) 3) 5) => ((nth-deriv f 3) 5))
   (test (((c-nth-deriv-f g) 3) 5) => ((nth-deriv g 3) 5))
   
   ; c-nth-deriv-n(n)
   (test (((c-nth-deriv-n 3) f) 5) => ((nth-deriv f 3) 5))
   (test (((c-nth-deriv-n 3) g) 5) => ((nth-deriv g 3) 5))
   
  ))

(display "q2-tests:\t\t")
(q2-tests)
