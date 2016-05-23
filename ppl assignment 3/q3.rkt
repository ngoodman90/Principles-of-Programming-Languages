#lang racket

(provide (all-defined-out))

; +-----------------------+
; |    Answer Q3-a below  |
; +-----------------------+

;Signature: empty-lzl()
;Type: 
;Purpose: 
;Pre-conditions:
;Tests:
(define empty-lzl empty)



;Signature: empty-lzl?()
;Type: 
;Purpose: 
;Pre-conditions:
;Tests:
(define empty-lzl? empty?)



;Signature: cons-lzl(x,lzl)
;Type: 
;Purpose: 
;Pre-conditions:
;Tests:
(define cons-lzl cons)


;Signature: head()
;Type: 
;Purpose: 
;Pre-conditions:
;Tests:
(define head car)



;Signature: tail(lz-lst)
;Type: 
;Purpose: 
;Pre-conditions:
;Tests:
(define tail
  (lambda (lz-lst)
    ((cdr lz-lst))
  ))




;Signature: take(lz-lst, n)
;Type: 
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
;Type: 
;Purpose: 
;Pre-conditions:
;Tests:
(define nth
  (lambda (lz-lst n)
    (if (= n 0)
        (head lz-lst)
        (nth (tail lz-lst) (sub1 n)))
    ))

;Signature: derive(f)
;Type: 
;Purpose: 
;Pre-conditions:
;Tests:
(define derive
   (lambda (f)
      (lambda (x)
        (let ((dx 0.001))
          (/ (- (f (+ x dx)) (f x))
              dx)))))


;Signature: lazy-function-root(f,x0)
;Type: 
;Purpose: 
;Pre-conditions:
;Tests:
(define lazy-function-root
  (lambda (f x0)
       (let  ((x1 (- x0 (/ (f x0) ((derive f) x0)))))
            (cons-lzl x0
                 (lambda ()
                   (lazy-function-root f x1))))))




