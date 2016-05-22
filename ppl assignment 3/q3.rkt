#lang racket

(provide (all-defined-out))

; +-----------------------+
; |    Answer Q3-a below  |
; +-----------------------+


;Signature: empty-lzl()
;Type: [() -> LzL]
;Pre-condition: none
(define empty-lzl empty)

;Signature: empty-lzl?(lz-ist)
;Type: [T -> Boolean]
(define empty-lzl? empty?)

;Signature: cons-lzl(x,lzl)
;Type: [T*LzL -> LzL]
;Pre-condition: lzl=empty-lzl or <closure () Lazy-list>
(define cons-lzl cons)

;Signature: head(lz-ist)
;Type: [LzL -> T] ;that is, the type is [T*[Empty -> LzL] -> T]
;Pre-condition: lzl-lst is not empty: (not (empty-lzl? lzl-lst)))
(define head car)

;Signature: tail(lz-ist)
;Type: [LzL -> T] ;that is, the type is [T*[Empty -> LzL] -> T]
;Pre-condition: lzl-lst is not empty: (not (empty-lzl? lzl-lst)))
(define tail
  (lambda (lz-lst)
    ((cdr lz-lst))
  ))


;Signature: take(lz-lst,n)
;Type: [LzL*Number -> List]
;Comment: If n > length(lz-lst) then the result is lz-lst as a List
(define take
  (lambda (lz-lst n)
    (if (or (= n 0) (empty-lzl? lz-lst))
        empty-lzl
        (cons (head lz-lst)
              (take (tail lz-lst) (sub1 n))))
))


;Signature: nth(lz-lst,n)
;Type: [LzL*Number -> T]
;Pre-condition: n < length(lz-lst)
(define nth
  (lambda (lz-lst n)
    (if (= n 0)
        (head lz-lst)
        (nth (tail lz-lst) (sub1 n)))
    ))

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


; Signature: lazy-function-root(f,x0)
; Type: [[Number -> Number] * Number * Number -> Lzl]
; Purpose: approximate a root of 'f' using the initial guess 'x0'
; Pre-conditions:
; Tests:
(define lazy-function-root
  (lambda (f x0)
       (let  ((x1 (- x0 (/ (f x0) ((derive f) x0)))))
            (cons-lzl x0
                 (lambda ()
                   (lazy-function-root f x1))))))




