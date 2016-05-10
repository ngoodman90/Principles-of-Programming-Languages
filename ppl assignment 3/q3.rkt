#lang racket

(provide (all-defined-out))

; +-----------------------+
; |    Answer Q3-a below  |
; +-----------------------+


;Signature: empty-lzl()
;Type: [() -> LzL]
;Pre-condition: none
(define empty-lzl empty)

;Signature: cons-lzl(x,lzl)
;Type: [T*LzL -> LzL]
;Pre-condition: lzl=empty-lzl or <closure () Lazy-list>
(define cons-lzl cons)

;Signature: derive(f dx)
;Purpose: to construct a procedure that computes the derivative
;dx approximation of a function:
;derive(f dx)(x) = (f(x+dx) - f(x) )/ dx
;Type: [[Number -> Number]*Number -> [Number -> Number]]
;Pre-conditions: 0 < dx < 1
;Tests: ((derive cube 0.001) 5) ==> ~75
(define derive
   (lambda (f dx)
      (lambda (x)
          (/ (- (f (+ x dx)) (f x))
              dx))))


; Signature: lazy-function-root(f,x0)
; Type: [[Number -> Number] * Number * Number -> Lzl]
; Purpose: approximate a root of 'f' using the initial guess 'x0'
; Pre-conditions:
; Tests:
(define lazy-function-root
  (lambda (f x0 eps)
    (if (< (abs (f x0)) eps)
       (cons-lzl x0 (lambda () empty-lzl))
       (let  ((x1 (- x0 (/ (f x0) ((derive f 0.001) x0)))))
            (cons-lzl x0
                 (lambda ()
                   (lazy-function-root f x1 eps)))))))




