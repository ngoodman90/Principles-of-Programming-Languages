#lang racket

(provide (all-defined-out))

; +-----------------------+
; |   Answer Q2-a below   |
; +-----------------------+


;Signature: derive(f dx)
;Type: [[Number -> Number]*Number -> [Number -> Number]]
;Purpose: to construct a procedure that computes the derivativedx approximation of a function:derive(f dx)(x) = (f(x+dx) - f(x) )/ dx
;Pre-conditions: 0 < dx < 1
;Tests: ((derive cube 0.001) 5) ==> ~75
(define derive
   (lambda (f)
      (lambda (x)
        (let ((dx 0.001))
          (/ (- (f (+ x dx)) (f x))
              dx)))))

; Signature: c-nth-deriv-f(f)
<<<<<<< HEAD
; Type: [[Number -> Number] -> [Number -> Number]]
; Purpose: derive a function n times
; Pre-conditions: n >= 0
; Tests: (((c-nth-deriv-f (lambda (x) x)) 1) 10) ==> ~0.9999999999994458
=======
; Type: [[Number -> Number] -> [Number -> Number]
; Purpose: a curried version of derive, function first
; Pre-conditions: none
; Tests: (((c-nth-deriv-f (lambda (x) x)) 1) 110) ==> 1.0000000000047748
>>>>>>> d0e0cc1bbcecaf90c21e7e0ce7e4a732fae4ac54
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
<<<<<<< HEAD
; Type: [Number -> [[Number -> Number] -> Number]]
; Purpose: derive a function n times
; Pre-conditions: n >= 0
; Tests: (((c-nth-deriv-n 1) (lambda (x) x)) 10) ==> ~0.9999999999994458
=======
; Type: [Number -> _____________]
; Purpose: a curried version of derive, n first
; Pre-conditions: n >= 0
; Tests:(((c-nth-deriv-n 1) (lambda (x) x))) 110) ==> 1.0000000000047748
>>>>>>> d0e0cc1bbcecaf90c21e7e0ce7e4a732fae4ac54
(define c-nth-deriv-n
  (lambda (n) 
    (if (= n 0)
        (lambda (f)
          f)
        (lambda (f)
          ((c-nth-deriv-n (- n 1)) (derive f))))))