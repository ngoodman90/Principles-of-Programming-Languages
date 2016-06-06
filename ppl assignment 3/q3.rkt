#lang racket

(provide (all-defined-out))

; +-----------------------+
; |    Answer Q3-a below  |
; +-----------------------+

;Signature: empty-lzl()
;Type: [ -> ]
;Purpose: 
;Pre-conditions:
;Tests:
(define empty-lzl empty)



;Signature: empty-lzl?()
;Type: [T -> Boolean]
;Purpose: 
;Pre-conditions:
;Tests:
(define empty-lzl? empty?)



;Signature: cons-lzl(x,lzl)
;Type: [T*LzL -> LzL]
;Purpose: 
;Pre-conditions: lzl=empty-lzl or <closure () Lazy-list>
;Tests:
(define cons-lzl cons)


;Signature: head()
;Type: [LzL -> T] ;that is, the type is [T*[Empty -> LzL] -> T]
;Purpose: 
;Pre-conditions: [LzL -> T] ;that is, the type is [T*[Empty -> LzL] -> T]
;Tests:
(define head car)



;Signature: tail(lz-lst)
;Type: [LzL -> T] ;that is, the type is [T*[Empty -> LzL] -> T]
;Purpose: 
;Pre-conditions:  lzl-lst is not empty: (not (empty-lzl? lzl-lst)))
;Tests:
(define tail
  (lambda (lz-lst)
    ((cdr lz-lst))
  ))




;Signature: take(lz-lst, n)
;Type: [LzL*Number -> List]
;Purpose: 
;Pre-conditions:
;Tests:
(define take
  (lambda (lz-lst n)
    (if (or (= n 0) (empty-lzl? lz-lst))
        empty-lzl
        (cons (head lz-lst)
              (take (tail lz-lst) (sub1 n))))
))


;Signature: nth(lz-lst, n)
;Type: [LzL*Number -> T]
;Purpose: 
;Pre-conditions: [LzL*Number -> T]
;Tests:
(define nth
  (lambda (lz-lst n)
    (if (= n 0)
        (head lz-lst)
        (nth (tail lz-lst) (sub1 n)))
    ))

;Signature: derive(f)
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


;Signature: lazy-function-root(f,x0)
;Type: [[Number -> Number]*Number -> [Number -> Number]]
;Purpose: 
;Pre-conditions:
;Tests:
(define lazy-function-root
  (lambda (f x0)
       (let  ((x1 (- x0 (/ (f x0) ((derive f) x0)))))
            (cons-lzl x0
                 (lambda ()
                   (lazy-function-root f x1))))))




