#lang racket

(provide (all-defined-out))

; +-----------------------+
; |   Answer Q2-a below   |
; +-----------------------+


;Signature: derive(f dx)
;Purpose: to construct a procedure that computes the derivative
;dx approximation of a function:
;derive(f dx)(x) = (f(x+dx) - f(x) )/ dx
;Type: [[Number -> Number]*Number -> [Number -> Number]]
;Pre-conditions: 0 < dx < 1
;Tests: ((derive cube 0.001) 5) ==> ~75
(define derive
   (lambda (f)
      (lambda (x)
        (let ((dx 0.001))
          (/ (- (f (+ x dx)) (f x))
              dx)))))

; Signature: c-nth-deriv-f(f)
; Type: [[Number -> Number] -> _____________]
; Purpose: 
; Pre-conditions:
; Tests:
(define c-nth-deriv-f
  (lambda (f) 
    (let ([f1 (derive f)])
          (lambda (n)
            (if (= n 0)
                f
                ((c-nth-deriv-f f1) (- n 1)))))))

; +-----------------------+
; |   Answer Q2-b below   |
; +-----------------------+

; Signature: c-nth-deriv-n(n)
; Type: [Number -> _____________]
; Purpose: 
; Pre-conditions:
; Tests:
(define c-nth-deriv-n
  (lambda (n) 
    (if (= n 0)
        (lambda (f)
          f)
        (lambda (f)
          ((c-nth-deriv-n (- n 1)) (derive f))))))